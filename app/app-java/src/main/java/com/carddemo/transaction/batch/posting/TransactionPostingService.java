package com.carddemo.transaction.batch.posting;

import com.carddemo.transaction.batch.validation.XrefAccountLookup;
import com.carddemo.transaction.cobol.CobolFileException;
import com.carddemo.transaction.cobol.FileStatus;
import com.carddemo.transaction.cobol.VsamIo;
import com.carddemo.transaction.domain.Account;
import com.carddemo.transaction.domain.CardXref;
import com.carddemo.transaction.domain.DailyTransaction;
import com.carddemo.transaction.domain.Transaction;
import com.carddemo.transaction.domain.TransactionCategoryBalance;
import com.carddemo.transaction.domain.TransactionCategoryBalanceId;
import com.carddemo.transaction.io.Db2Timestamp;
import com.carddemo.transaction.io.RejectRecord;
import com.carddemo.transaction.repository.AccountRepository;
import com.carddemo.transaction.repository.TransactionCategoryBalanceRepository;
import com.carddemo.transaction.repository.TransactionRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.Clock;
import java.util.Optional;

/**
 * Paragraph-for-paragraph port of the CBTRN02C per-record logic. Runs inside the Spring Batch chunk
 * transaction, so all JPA writes for a chunk commit (or roll back) together.
 */
@Service
public class TransactionPostingService {

    private static final Logger log = LoggerFactory.getLogger(TransactionPostingService.class);

    public static final int REASON_INVALID_CARD = 100;
    public static final int REASON_ACCOUNT_NOT_FOUND = 101;
    public static final int REASON_OVERLIMIT = 102;
    public static final int REASON_AFTER_EXPIRATION = 103;
    public static final int REASON_ACCOUNT_REWRITE_NOT_FOUND = 109;

    private final XrefAccountLookup lookup;
    private final AccountRepository accounts;
    private final TransactionCategoryBalanceRepository tcatbals;
    private final TransactionRepository transactions;
    private final Clock clock;

    public TransactionPostingService(XrefAccountLookup lookup, AccountRepository accounts,
                                     TransactionCategoryBalanceRepository tcatbals,
                                     TransactionRepository transactions, Clock clock) {
        this.lookup = lookup;
        this.accounts = accounts;
        this.tcatbals = tcatbals;
        this.transactions = transactions;
        this.clock = clock;
    }

    /** WS-VALIDATION-TRAILER plus the records read during validation (XREF / ACCOUNT record areas). */
    public record Validation(int failReason, String failReasonDesc, CardXref xref, Account account) {
        public boolean passed() {
            return failReason == 0;
        }
    }

    /** Main loop body: 1500-VALIDATE-TRAN, then 2000-POST-TRANSACTION or 2500-WRITE-REJECT-REC. */
    public Optional<RejectRecord> process(DailyTransaction tran) {
        Validation v = validateTran(tran);
        if (v.passed()) {
            postTransaction(tran, v);
            return Optional.empty();
        }
        return Optional.of(writeRejectRec(tran, v));
    }

    /** 1500-VALIDATE-TRAN. */
    public Validation validateTran(DailyTransaction tran) {
        Optional<CardXref> xref = lookup.readXref(tran.cardNum());               // 1500-A-LOOKUP-XREF
        if (xref.isEmpty()) {
            return new Validation(REASON_INVALID_CARD, "INVALID CARD NUMBER FOUND", null, null);
        }
        return lookupAcct(tran, xref.get());
    }

    /** 1500-B-LOOKUP-ACCT. Both checks run; when both fail the later one (103) wins, as in COBOL. */
    Validation lookupAcct(DailyTransaction tran, CardXref xref) {
        Optional<Account> found = lookup.readAccount(xref.getAcctId());
        if (found.isEmpty()) {
            return new Validation(REASON_ACCOUNT_NOT_FOUND, "ACCOUNT RECORD NOT FOUND", xref, null);
        }
        Account acct = found.get();
        int reason = 0;
        String desc = "";
        BigDecimal tempBal = acct.getCurrCycCredit().subtract(acct.getCurrCycDebit()).add(tran.amt());
        if (acct.getCreditLimit().compareTo(tempBal) < 0) {
            reason = REASON_OVERLIMIT;
            desc = "OVERLIMIT TRANSACTION";
        }
        // alphanumeric compare of X(10) against DALYTRAN-ORIG-TS(1:10)
        if (acct.getExpirationDate().compareTo(tran.origTs().substring(0, 10)) < 0) {
            reason = REASON_AFTER_EXPIRATION;
            desc = "TRANSACTION RECEIVED AFTER ACCT EXPIRATION";
        }
        return new Validation(reason, desc, xref, acct);
    }

    /** 2000-POST-TRANSACTION. */
    void postTransaction(DailyTransaction tran, Validation v) {
        Transaction t = new Transaction();
        t.setTranId(tran.id());
        t.setTypeCd(tran.typeCd());
        t.setCatCd(tran.catCd());
        t.setSource(tran.source());
        t.setDesc(tran.desc());
        t.setAmt(tran.amt());
        t.setMerchantId(tran.merchantId());
        t.setMerchantName(tran.merchantName());
        t.setMerchantCity(tran.merchantCity());
        t.setMerchantZip(tran.merchantZip());
        t.setCardNum(tran.cardNum());
        t.setOrigTs(tran.origTs());
        t.setProcTs(Db2Timestamp.now(clock));                                    // Z-GET-DB2-FORMAT-TIMESTAMP

        updateTcatbal(tran, v.xref());
        updateAccountRec(tran, v.account());
        writeTransactionFile(t);
    }

    /** 2700-UPDATE-TCATBAL with 2700-A-CREATE-TCATBAL-REC / 2700-B-UPDATE-TCATBAL-REC. */
    void updateTcatbal(DailyTransaction tran, CardXref xref) {
        TransactionCategoryBalanceId key = new TransactionCategoryBalanceId(xref.getAcctId(), tran.typeCd(), tran.catCd());
        Optional<TransactionCategoryBalance> existing =
                VsamIo.perform("TCATBALF", "ERROR READING TRANSACTION BALANCE FILE", () -> tcatbals.findById(key));
        TransactionCategoryBalance bal;
        if (existing.isEmpty()) {
            log.info("TCATBAL record not found for key : {}.. Creating.", key);
            bal = new TransactionCategoryBalance(key);
            bal.setBalance(bal.getBalance().add(tran.amt()));
            VsamIo.perform("TCATBALF", "ERROR WRITING TRANSACTION BALANCE FILE", () -> tcatbals.save(bal));
        } else {
            bal = existing.get();
            bal.setBalance(bal.getBalance().add(tran.amt()));
            VsamIo.perform("TCATBALF", "ERROR REWRITING TRANSACTION BALANCE FILE", () -> tcatbals.save(bal));
        }
    }

    /** 2800-UPDATE-ACCOUNT-REC. */
    void updateAccountRec(DailyTransaction tran, Account acct) {
        acct.setCurrBal(acct.getCurrBal().add(tran.amt()));
        if (tran.amt().signum() >= 0) {
            acct.setCurrCycCredit(acct.getCurrCycCredit().add(tran.amt()));
        } else {
            acct.setCurrCycDebit(acct.getCurrCycDebit().add(tran.amt()));
        }
        boolean exists = VsamIo.perform("ACCTFILE", "ERROR REWRITING ACCOUNT FILE",
                () -> accounts.existsById(acct.getAcctId()));
        if (!exists) {
            // REWRITE INVALID KEY: COBOL only sets reason 109 here; the transaction is still written.
            log.warn("{} ACCOUNT RECORD NOT FOUND - ACCOUNT {} NOT REWRITTEN", REASON_ACCOUNT_REWRITE_NOT_FOUND,
                    acct.getAcctId());
            return;
        }
        VsamIo.perform("ACCTFILE", "ERROR REWRITING ACCOUNT FILE", () -> accounts.save(acct));
    }

    /** 2900-WRITE-TRANSACTION-FILE. A duplicate TRAN-ID is status 22 and abends, like the KSDS WRITE. */
    void writeTransactionFile(Transaction t) {
        boolean duplicate = VsamIo.perform("TRANFILE", "ERROR WRITING TO TRANSACTION FILE",
                () -> transactions.existsById(t.getTranId()));
        if (duplicate) {
            throw new CobolFileException("TRANFILE", FileStatus.DUPLICATE_KEY,
                    "ERROR WRITING TO TRANSACTION FILE - DUPLICATE TRAN-ID " + t.getTranId(), null);
        }
        VsamIo.perform("TRANFILE", "ERROR WRITING TO TRANSACTION FILE", () -> transactions.save(t));
    }

    /** 2500-WRITE-REJECT-REC (record image; the DALYREJS writer performs the WRITE). */
    RejectRecord writeRejectRec(DailyTransaction tran, Validation v) {
        log.warn("REJECTED TRANSACTION {} REASON {} {}", tran.id(), v.failReason(), v.failReasonDesc());
        return new RejectRecord(tran.rawRecord(), v.failReason(), v.failReasonDesc());
    }
}
