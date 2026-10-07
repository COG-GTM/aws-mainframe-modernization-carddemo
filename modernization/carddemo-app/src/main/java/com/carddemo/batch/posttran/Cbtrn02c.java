package com.carddemo.batch.posttran;

import com.carddemo.account.AccountRecord;
import com.carddemo.batch.harness.KsdsInput;
import com.carddemo.batch.harness.RecordSink;
import com.carddemo.batch.harness.ReturnCode;
import com.carddemo.batch.harness.Sysout;
import com.carddemo.card.CardXrefRecord;
import com.carddemo.common.codec.CobolNumeric;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.file.FileStatus;
import com.carddemo.common.file.FileStatusException;
import com.carddemo.transaction.DailyTransactionRecord;
import com.carddemo.transaction.TranCatBalanceId;
import com.carddemo.transaction.TranCatBalanceRecord;
import com.carddemo.transaction.TransactionRecord;
import java.math.BigDecimal;
import java.time.Clock;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Locale;
import java.util.Optional;

/**
 * {@code CBTRN02C} (POSTTRAN STEP15): posts each DALYTRAN record. Validation in program order: card in XREFFILE
 * (100), account in ACCTFILE (101), {@code ACCT-CREDIT-LIMIT >= ACCT-CURR-CYC-CREDIT - ACCT-CURR-CYC-DEBIT +
 * DALYTRAN-AMT} (102), {@code ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS(1:10)} (103; checked after 102 and overwrites
 * it). A valid record updates or creates its TCATBALF row, adds the amount to {@code ACCT-CURR-BAL} and to the cycle
 * credit ({@code >= 0}) or debit ({@code < 0}) total, and is written to TRANFILE with {@code TRAN-PROC-TS} from the
 * clock; a rejected one goes to DALYREJS as the 350-byte record plus an 80-byte trailer (reason {@code 9(04)} +
 * description {@code X(76)}). RC 4 when any record was rejected. Arithmetic: BigDecimal, high-order truncation into
 * the receiving PIC, no rounding (ADR-0004/0005). Each record's reads and updates run in one {@link UnitOfWork}.
 */
public final class Cbtrn02c {

    public static final String PROGRAM = "CBTRN02C";
    public static final String DALYTRAN = "DALYTRAN";
    public static final String TRANFILE = "TRANFILE";
    public static final String XREFFILE = "XREFFILE";
    public static final String DALYREJS = "DALYREJS";
    public static final String ACCTFILE = "ACCTFILE";
    public static final String TCATBALF = "TCATBALF";
    public static final int REJECT_LRECL = 430;
    public static final int TRAILER_LENGTH = 80;

    public static final int INVALID_CARD = 100;
    public static final int ACCOUNT_NOT_FOUND = 101;
    public static final int OVERLIMIT = 102;
    public static final int EXPIRED = 103;

    /** {@code Z-GET-DB2-FORMAT-TIMESTAMP}: {@code YYYY-MM-DD-HH.MM.SS.hh0000} (hundredths, then '0000'). */
    static final DateTimeFormatter PROC_TS = DateTimeFormatter.ofPattern("yyyy-MM-dd-HH.mm.ss.SS'0000'", Locale.ROOT);

    /** The database transaction around one record's validation reads and posting updates. */
    @FunctionalInterface
    public interface UnitOfWork {
        UnitOfWork NONE = Runnable::run;

        void run(Runnable work);
    }

    public record Validation(int reason, String description) {
        static final Validation OK = new Validation(0, "");

        public boolean valid() {
            return reason == 0;
        }

        /** {@code WS-VALIDATION-TRAILER}: {@code 9(04)} + {@code X(76)}. */
        public String trailer() {
            String desc = description.length() > TRAILER_LENGTH - 4 ? description.substring(0, TRAILER_LENGTH - 4)
                    : description;
            return String.format(Locale.ROOT, "%04d%-76s", reason, desc);
        }
    }

    public record Result(long processed, long rejected, ReturnCode returnCode) {
        public long posted() {
            return processed - rejected;
        }
    }

    private record Checked(Validation validation, CardXrefRecord xref, AccountRecord account) {
    }

    private final KsdsInput dalytran;
    private final KeyedDataset<String, TransactionRecord> tranfile;
    private final KeyedDataset<String, CardXrefRecord> xreffile;
    private final RecordSink dalyrejs;
    private final KeyedDataset<Long, AccountRecord> acctfile;
    private final KeyedDataset<TranCatBalanceId, TranCatBalanceRecord> tcatbalf;
    private final Sysout sysout;
    private final Clock clock;
    private final UnitOfWork unitOfWork;

    public Cbtrn02c(KsdsInput dalytran, KeyedDataset<String, TransactionRecord> tranfile,
                    KeyedDataset<String, CardXrefRecord> xreffile, RecordSink dalyrejs,
                    KeyedDataset<Long, AccountRecord> acctfile,
                    KeyedDataset<TranCatBalanceId, TranCatBalanceRecord> tcatbalf, Sysout sysout, Clock clock,
                    UnitOfWork unitOfWork) {
        this.dalytran = dalytran;
        this.tranfile = tranfile;
        this.xreffile = xreffile;
        this.dalyrejs = dalyrejs;
        this.acctfile = acctfile;
        this.tcatbalf = tcatbalf;
        this.sysout = sysout;
        this.clock = clock;
        this.unitOfWork = unitOfWork;
    }

    public Result run() {
        sysout.display("START OF EXECUTION OF PROGRAM " + PROGRAM);
        io(dalytran::open, "ERROR OPENING DALYTRAN");
        io(tranfile::open, "ERROR OPENING TRANSACTION FILE");
        io(xreffile::open, "ERROR OPENING CROSS REF FILE");
        io(dalyrejs::open, "ERROR OPENING DALY REJECTS FILE");
        io(acctfile::open, "ERROR OPENING ACCOUNT MASTER FILE");
        io(tcatbalf::open, "ERROR OPENING TRANSACTION BALANCE FILE");
        long processed = 0;
        long rejected = 0;
        while (true) {
            Optional<FixedWidthRecord> next;
            try {
                next = dalytran.readNext();
            } catch (FileStatusException e) {
                throw sysout.ioAbend("ERROR READING DALYTRAN FILE", e);
            }
            if (next.isEmpty()) {
                break;
            }
            processed++;
            FixedWidthRecord image = next.get();
            DailyTransactionRecord tran = DailyTransactionRecord.MAPPER.fromRecord(image);
            Validation[] outcome = new Validation[1];
            unitOfWork.run(() -> {
                Checked checked = validate(tran);
                if (checked.validation().valid()) {
                    post(tran, checked.xref(), checked.account());
                }
                outcome[0] = checked.validation();
            });
            if (!outcome[0].valid()) {
                rejected++;
                writeReject(image, outcome[0]);
            }
        }
        io(dalytran::close, "ERROR CLOSING DALYTRAN FILE");
        io(tranfile::close, "ERROR CLOSING TRANSACTION FILE");
        io(xreffile::close, "ERROR CLOSING CROSS REF FILE");
        io(dalyrejs::close, "ERROR CLOSING DAILY REJECTS FILE");
        io(acctfile::close, "ERROR CLOSING ACCOUNT FILE");
        io(tcatbalf::close, "ERROR CLOSING TRANSACTION BALANCE FILE");
        sysout.display(String.format(Locale.ROOT, "TRANSACTIONS PROCESSED :%09d", processed));
        sysout.display(String.format(Locale.ROOT, "TRANSACTIONS REJECTED  :%09d", rejected));
        ReturnCode rc = rejected > 0 ? ReturnCode.WARNING : ReturnCode.OK;
        sysout.display("END OF EXECUTION OF PROGRAM " + PROGRAM);
        return new Result(processed, rejected, rc);
    }

    /** {@code 1500-VALIDATE-TRAN}. */
    private Checked validate(DailyTransactionRecord tran) {
        Optional<CardXrefRecord> xref = xreffile.read(tran.cardNum());
        if (xref.isEmpty()) {
            return new Checked(new Validation(INVALID_CARD, "INVALID CARD NUMBER FOUND"), null, null);
        }
        Optional<AccountRecord> account = acctfile.read(xref.get().acctId());
        if (account.isEmpty()) {
            return new Checked(new Validation(ACCOUNT_NOT_FOUND, "ACCOUNT RECORD NOT FOUND"), xref.get(), null);
        }
        return new Checked(validate(tran, account.get()), xref.get(), account.get());
    }

    /** The credit-limit and expiration checks of {@code 1500-B-LOOKUP-ACCT}, in program order. */
    static Validation validate(DailyTransactionRecord tran, AccountRecord account) {
        Validation result = Validation.OK;
        BigDecimal tempBal = CobolNumeric.truncate(
                account.currCycCredit().subtract(account.currCycDebit()).add(tran.amount()), 11, 2, true);
        if (account.creditLimit().compareTo(tempBal) < 0) {
            result = new Validation(OVERLIMIT, "OVERLIMIT TRANSACTION");
        }
        if (pad(account.expirationDate(), 10).compareTo(pad(tran.origTs(), 10).substring(0, 10)) < 0) {
            result = new Validation(EXPIRED, "TRANSACTION RECEIVED AFTER ACCT EXPIRATION");
        }
        return result;
    }

    /** {@code 2000-POST-TRANSACTION}: 2700 TCATBALF, 2800 ACCTFILE, 2900 TRANFILE. */
    private void post(DailyTransactionRecord tran, CardXrefRecord xref, AccountRecord account) {
        BigDecimal amount = tran.amount();
        TransactionRecord transaction = new TransactionRecord(tran.tranId(), tran.tranTypeCd(), tran.tranCatCd(),
                tran.source(), tran.description(), amount, tran.merchantId(), tran.merchantName(),
                tran.merchantCity(), tran.merchantZip(), tran.cardNum(), tran.origTs(),
                PROC_TS.format(LocalDateTime.now(clock)));

        TranCatBalanceId key = new TranCatBalanceId(xref.acctId(), tran.tranTypeCd(), tran.tranCatCd());
        Optional<TranCatBalanceRecord> balance;
        try {
            balance = tcatbalf.read(key);
        } catch (FileStatusException e) {
            throw sysout.ioAbend("ERROR READING TRANSACTION BALANCE FILE", e);
        }
        if (balance.isEmpty()) {
            sysout.display("TCATBAL record not found for key : " + tcatKey(key) + ".. Creating.");
            TranCatBalanceRecord created = new TranCatBalanceRecord(key.acctId(), key.tranTypeCd(), key.tranCatCd(),
                    CobolNumeric.truncate(amount, 11, 2, true));
            io(() -> tcatbalf.write(created), "ERROR WRITING TRANSACTION BALANCE FILE");
        } else {
            TranCatBalanceRecord b = balance.get();
            TranCatBalanceRecord updated = new TranCatBalanceRecord(b.acctId(), b.tranTypeCd(), b.tranCatCd(),
                    CobolNumeric.truncate(b.balance().add(amount), 11, 2, true));
            io(() -> {
                if (!tcatbalf.rewrite(updated)) {
                    throw new FileStatusException(TCATBALF, "REWRITE", FileStatus.RECORD_NOT_FOUND);
                }
            }, "ERROR REWRITING TRANSACTION BALANCE FILE");
        }

        boolean credit = amount.signum() >= 0;
        AccountRecord updated = new AccountRecord(account.acctId(), account.activeStatus(),
                CobolNumeric.truncate(account.currBal().add(amount), 12, 2, true), account.creditLimit(),
                account.cashCreditLimit(), account.openDate(), account.expirationDate(), account.reissueDate(),
                credit ? CobolNumeric.truncate(account.currCycCredit().add(amount), 12, 2, true)
                        : account.currCycCredit(),
                credit ? account.currCycDebit()
                        : CobolNumeric.truncate(account.currCycDebit().add(amount), 12, 2, true),
                account.addrZip(), account.groupId());
        // REWRITE INVALID KEY only sets reason 109 in WS-VALIDATION-TRAILER, which nothing reads afterwards.
        acctfile.rewrite(updated);

        io(() -> tranfile.write(transaction), "ERROR WRITING TO TRANSACTION FILE");
    }

    /** {@code 2500-WRITE-REJECT-REC}. */
    private void writeReject(FixedWidthRecord image, Validation validation) {
        byte[] data = image.bytes();
        byte[] trailer = image.encoding().encode(validation.trailer());
        byte[] reject = new byte[data.length + trailer.length];
        System.arraycopy(data, 0, reject, 0, data.length);
        System.arraycopy(trailer, 0, reject, data.length, trailer.length);
        io(() -> dalyrejs.write(new FixedWidthRecord(reject, image.encoding())), "ERROR WRITING TO REJECTS FILE");
    }

    /** {@code FD-TRAN-CAT-KEY}: {@code 9(11)} + {@code X(02)} + {@code 9(04)}. */
    static String tcatKey(TranCatBalanceId key) {
        return String.format(Locale.ROOT, "%011d%-2s%04d", key.acctId(), key.tranTypeCd(), key.tranCatCd());
    }

    private static String pad(String value, int length) {
        String v = value == null ? "" : value;
        return v.length() >= length ? v : v + " ".repeat(length - v.length());
    }

    private void io(Runnable operation, String message) {
        try {
            operation.run();
        } catch (FileStatusException e) {
            throw sysout.ioAbend(message, e);
        }
    }
}
