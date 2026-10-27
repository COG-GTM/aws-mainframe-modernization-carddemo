package com.carddemo.modernization.posting;

import com.carddemo.modernization.records.*;
import com.carddemo.modernization.records.codec.ZonedDecimal;
import com.carddemo.modernization.records.codec.ZonedDecimalCodec;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.charset.Charset;
import java.time.Clock;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Objects;
import java.util.Arrays;
import java.util.Locale;

public final class TransactionPoster {
    /** Missing reads return null; other I/O failures throw. Writes throw on failure. */
    public interface Store {
        /** Open fresh transaction and reject outputs, preserving all input/master records. */
        void openOutputs();
        CardXrefRecord findCard(String cardNumber);
        AccountRecord findAccount(String accountId);
        TranCatBalanceRecord findCategory(String accountId, String type, String category);
        void saveCategory(TranCatBalanceRecord record, boolean create);
        /** False represents INVALID KEY, not an exception. */
        boolean rewriteAccount(AccountRecord record);
        /** Write an encoded 350-byte record; merchant ID bytes may be non-numeric. */
        void writeTransaction(byte[] record);
        void writeReject(byte[] record);
    }

    public record Result(long posted, long rejected, int returnCode) {}

    private final Clock clock;
    private final Charset charset;
    private static final DateTimeFormatter TIMESTAMP = DateTimeFormatter.ofPattern("uuuu-MM-dd-HH.mm.ss.SS'0000'", Locale.ROOT);

    public TransactionPoster(Clock clock, Charset charset) {
        this.clock = Objects.requireNonNull(clock);
        this.charset = Objects.requireNonNull(charset);
    }

    public Result post(Iterable<byte[]> dailyRecords, Store store) {
        // POST-01: app/cbl/CBTRN02C.cbl:256,293, OPEN OUTPUT for each batch run.
        store.openOutputs();
        long posted = 0;
        long rejected = 0;
        for (byte[] bytes : dailyRecords) {
            if (bytes.length != DailyTransactionRecord.CODEC.length()) {
                throw new IllegalArgumentException("Expected 350-byte daily record");
            }
            int reason = 0;
            String description = "";
            // POST-03/04/05: app/cbl/CBTRN02C.cbl:370-400, xref precedes account.
            // app/cpy/CVTRA06Y.cpy:5-16: card offset 262, amount offset 132, original timestamp offset 278.
            CardXrefRecord xref = store.findCard(new String(bytes, 262, 16, charset));
            AccountRecord a = null;
            if (xref == null) {
                reason = 100;
                description = "INVALID CARD NUMBER FOUND";
            } else {
                a = store.findAccount(xref.accountId());
                if (a == null) {
                    reason = 101;
                    description = "ACCOUNT RECORD NOT FOUND";
                } else {
                    // POST-06: app/cbl/CBTRN02C.cbl:403-413, S9(09)V99 truncates high digits.
                    BigDecimal temporary = pic(a.cycleCredit().value().subtract(a.cycleDebit().value())
                            .add(new ZonedDecimalCodec(11, 2).decode(Arrays.copyOfRange(bytes, 132, 143), charset).value()), 9);
                    if (a.creditLimit().value().compareTo(temporary) < 0) {
                        reason = 102;
                        description = "OVERLIMIT TRANSACTION";
                    }
                    // POST-07: app/cbl/CBTRN02C.cbl:414-420, expiry intentionally overwrites 102.
                    if (a.expirationDate().compareTo(new String(bytes, 278, 10, charset)) < 0) {
                        reason = 103;
                        description = "TRANSACTION RECEIVED AFTER ACCT EXPIRATION";
                    }
                }
            }
            if (reason != 0) {
                // POST-08: app/cbl/CBTRN02C.cbl:446-452, original 350 bytes plus 80-byte trailer.
                byte[] reject = RejectRecord.CODEC.encode(new RejectRecord(
                        "", String.format(Locale.ROOT, "%04d", reason), description), charset);
                System.arraycopy(bytes, 0, reject, 0, bytes.length);
                store.writeReject(reject);
                rejected++;
                continue;
            }
            // POST-10: app/cbl/CBTRN02C.cbl:431 moves DISPLAY merchant ID bytes without arithmetic.
            // app/cpy/CVTRA05Y.cpy:5-11 and CVTRA06Y.cpy:5-11: offset 143, length 9.
            byte[] decodable = bytes.clone();
            System.arraycopy("000000000".getBytes(charset), 0, decodable, 143, 9);
            DailyTransactionRecord t = DailyTransactionRecord.CODEC.decode(decodable, charset);
            // POST-10/18: app/cbl/CBTRN02C.cbl:424-438,692-705. Filler remains spaces.
            TransactionRecord transaction = new TransactionRecord(t.id(), t.typeCode(), t.categoryCode(),
                    t.source(), t.description(), new ZonedDecimal(pic(t.amount().value(), 9)), t.merchantId(),
                    t.merchantName(), t.merchantCity(), t.merchantZip(), t.cardNumber(),
                    t.originalTimestamp(), TIMESTAMP.format(LocalDateTime.now(clock)), "");
            byte[] encodedTransaction = TransactionRecord.CODEC.encode(transaction, charset);
            System.arraycopy(bytes, 143, encodedTransaction, 143, 9);
            // POST-11: app/cbl/CBTRN02C.cbl:440-442, ordered effects with no rollback.
            TranCatBalanceRecord category = store.findCategory(xref.accountId(), t.typeCode(), t.categoryCode());
            boolean create = category == null;
            // POST-12/13/14: app/cbl/CBTRN02C.cbl:467-542.
            store.saveCategory(new TranCatBalanceRecord(xref.accountId(), t.typeCode(), t.categoryCode(),
                    new ZonedDecimal(pic((create ? BigDecimal.ZERO : category.balance().value())
                            .add(t.amount().value()), 9)), create ? "" : category.filler()), create);
            // POST-15: app/cbl/CBTRN02C.cbl:545-552, debit totals accumulate negative amounts.
            BigDecimal amount = t.amount().value();
            AccountRecord updated = new AccountRecord(a.acctId(), a.activeStatus(),
                    add(a.currentBalance(), amount), a.creditLimit(), a.cashCreditLimit(), a.openDate(),
                    a.expirationDate(), a.reissueDate(),
                    amount.signum() >= 0 ? add(a.cycleCredit(), amount) : a.cycleCredit(),
                    amount.signum() < 0 ? add(a.cycleDebit(), amount) : a.cycleDebit(),
                    a.addressZip(), a.groupId(), a.filler());
            // POST-16: app/cbl/CBTRN02C.cbl:554-560, reason 109 is not acted on: no reject or RC change.
            store.rewriteAccount(updated);
            // POST-17/19: app/cbl/CBTRN02C.cbl:562-579, write errors propagate after prior updates.
            store.writeTransaction(encodedTransaction);
            posted++;
        }
        // POST-09: app/cbl/CBTRN02C.cbl:229-231.
        return new Result(posted, rejected, rejected > 0 ? 4 : 0);
    }

    private static ZonedDecimal add(ZonedDecimal value, BigDecimal amount) {
        return new ZonedDecimal(pic(value.value().add(amount), 10));
    }

    private static BigDecimal pic(BigDecimal value, int integerDigits) {
        return value.setScale(2, RoundingMode.DOWN).remainder(BigDecimal.TEN.pow(integerDigits));
    }
}
