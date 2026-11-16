package com.carddemo.batch.jobs.posttran;

import com.carddemo.batch.io.KsdsFile;
import com.carddemo.batch.model.AccountRecord;
import com.carddemo.batch.model.CardXrefRecord;
import com.carddemo.batch.model.DailyTransactionRecord;
import com.carddemo.batch.model.TranCatBalRecord;
import com.carddemo.batch.model.TranCatBalRecord.TranCatKey;
import com.carddemo.batch.model.TransactionRecord;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.time.Clock;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Optional;

/**
 * Business logic of CBTRN02C (paragraphs 1500-VALIDATE-TRAN through 2900-WRITE-TRANSACTION-FILE)
 * applied to one DALYTRAN record against the in-memory indexed files.
 */
public class PostingEngine {

    public static final int REASON_INVALID_CARD = 100;
    public static final int REASON_ACCOUNT_NOT_FOUND = 101;
    public static final int REASON_OVERLIMIT = 102;
    public static final int REASON_AFTER_EXPIRATION = 103;

    private final KsdsFile<String, CardXrefRecord> xrefFile;
    private final KsdsFile<Long, AccountRecord> accountFile;
    private final KsdsFile<TranCatKey, TranCatBalRecord> tcatbalFile;
    private final KsdsFile<String, TransactionRecord> transactFile;
    private final Clock clock;
    private final List<String> sysout = new ArrayList<>();
    private long transactionCount;
    private long rejectCount;
    // FILLER of the WORKING-STORAGE TRAN-CAT-BAL-RECORD: INITIALIZE skips FILLER, so a created
    // TCATBAL record keeps the filler of the last record read INTO it (spaces before any read).
    private String lastTcatFiller = "";

    public PostingEngine(KsdsFile<String, CardXrefRecord> xrefFile,
                         KsdsFile<Long, AccountRecord> accountFile,
                         KsdsFile<TranCatKey, TranCatBalRecord> tcatbalFile,
                         KsdsFile<String, TransactionRecord> transactFile,
                         Clock clock) {
        this.xrefFile = xrefFile;
        this.accountFile = accountFile;
        this.tcatbalFile = tcatbalFile;
        this.transactFile = transactFile;
        this.clock = clock;
    }

    /** Validates and posts one record; returns the reject to write to DALYREJS, if any. */
    public Optional<RejectRecord> process(DailyTransactionRecord tran) {
        transactionCount++;
        Validation validation = validate(tran);
        if (validation.reason() == 0) {
            post(tran, validation.xref());
            return Optional.empty();
        }
        rejectCount++;
        return Optional.of(new RejectRecord(tran, validation.reason(), validation.description()));
    }

    Validation validate(DailyTransactionRecord tran) {
        Optional<CardXrefRecord> xref = xrefFile.read(tran.dalytranCardNum());
        if (xref.isEmpty()) {
            return new Validation(null, REASON_INVALID_CARD, "INVALID CARD NUMBER FOUND");
        }
        Optional<AccountRecord> account = accountFile.read(xref.get().xrefAcctId());
        if (account.isEmpty()) {
            return new Validation(xref.get(), REASON_ACCOUNT_NOT_FOUND, "ACCOUNT RECORD NOT FOUND");
        }
        AccountRecord acct = account.get();
        int reason = 0;
        String description = "";
        BigDecimal tempBal = truncate(acct.acctCurrCycCredit()
                .subtract(acct.acctCurrCycDebit())
                .add(tran.dalytranAmt()), 9, 2);
        if (acct.acctCreditLimit().compareTo(tempBal) < 0) {
            reason = REASON_OVERLIMIT;
            description = "OVERLIMIT TRANSACTION";
        }
        // Alphanumeric compare, as in COBOL; a later failure overwrites an earlier one.
        if (acct.acctExpiraionDate().compareTo(tran.dalytranOrigTs().substring(0, 10)) < 0) {
            reason = REASON_AFTER_EXPIRATION;
            description = "TRANSACTION RECEIVED AFTER ACCT EXPIRATION";
        }
        return new Validation(xref.get(), reason, description);
    }

    private void post(DailyTransactionRecord tran, CardXrefRecord xref) {
        TransactionRecord out = new TransactionRecord(
                tran.dalytranId(), tran.dalytranTypeCd(), tran.dalytranCatCd(), tran.dalytranSource(),
                tran.dalytranDesc(), tran.dalytranAmt(), tran.dalytranMerchantId(), tran.dalytranMerchantName(),
                tran.dalytranMerchantCity(), tran.dalytranMerchantZip(), tran.dalytranCardNum(),
                tran.dalytranOrigTs(), db2Timestamp(), "");
        updateTcatbal(tran, xref);
        updateAccount(tran, xref);
        transactFile.write(out);
    }

    private void updateTcatbal(DailyTransactionRecord tran, CardXrefRecord xref) {
        TranCatKey key = new TranCatKey(xref.xrefAcctId(), tran.dalytranTypeCd(), tran.dalytranCatCd());
        Optional<TranCatBalRecord> existing = tcatbalFile.read(key);
        if (existing.isEmpty()) {
            sysout.add("TCATBAL record not found for key : " + key.encode() + ".. Creating.");
            tcatbalFile.write(new TranCatBalRecord(key, truncate(tran.dalytranAmt(), 9, 2), lastTcatFiller));
        } else {
            TranCatBalRecord rec = existing.get();
            lastTcatFiller = rec.filler();
            tcatbalFile.rewrite(rec.withTranCatBal(truncate(rec.tranCatBal().add(tran.dalytranAmt()), 9, 2)));
        }
    }

    private void updateAccount(DailyTransactionRecord tran, CardXrefRecord xref) {
        AccountRecord acct = accountFile.read(xref.xrefAcctId()).orElseThrow();
        BigDecimal amt = tran.dalytranAmt();
        acct = acct.withAcctCurrBal(truncate(acct.acctCurrBal().add(amt), 10, 2));
        if (amt.signum() >= 0) {
            acct = acct.withAcctCurrCycCredit(truncate(acct.acctCurrCycCredit().add(amt), 10, 2));
        } else {
            acct = acct.withAcctCurrCycDebit(truncate(acct.acctCurrCycDebit().add(amt), 10, 2));
        }
        accountFile.rewrite(acct);
    }

    /** Z-GET-DB2-FORMAT-TIMESTAMP: CURRENT-DATE as YYYY-MM-DD-HH.MM.SS.hh0000. */
    String db2Timestamp() {
        LocalDateTime now = LocalDateTime.now(clock);
        return String.format(Locale.ROOT, "%04d-%02d-%02d-%02d.%02d.%02d.%02d0000",
                now.getYear(), now.getMonthValue(), now.getDayOfMonth(),
                now.getHour(), now.getMinute(), now.getSecond(), now.getNano() / 10_000_000);
    }

    /** Store into PIC S9(integerDigits)V9(scale) without ON SIZE ERROR: high-order digits are lost. */
    static BigDecimal truncate(BigDecimal value, int integerDigits, int scale) {
        BigInteger unscaled = value.setScale(scale, RoundingMode.DOWN).unscaledValue();
        BigInteger kept = unscaled.abs().mod(BigInteger.TEN.pow(integerDigits + scale));
        return new BigDecimal(unscaled.signum() < 0 ? kept.negate() : kept, scale);
    }

    public List<String> sysout() {
        return sysout;
    }

    public long transactionCount() {
        return transactionCount;
    }

    public long rejectCount() {
        return rejectCount;
    }

    public int returnCode() {
        return rejectCount > 0 ? 4 : 0;
    }

    record Validation(CardXrefRecord xref, int reason, String description) {
    }
}
