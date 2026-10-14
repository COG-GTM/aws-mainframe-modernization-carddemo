package com.carddemo.batch.posttran.logic;

import com.carddemo.batch.posttran.cobol.Abend;
import com.carddemo.batch.posttran.cobol.CobolCompare;
import com.carddemo.batch.posttran.cobol.Db2Timestamp;
import com.carddemo.batch.posttran.cobol.Pic;
import com.carddemo.batch.posttran.cobol.Sysout;
import com.carddemo.batch.posttran.cobol.ZonedDecimal;
import com.carddemo.batch.posttran.domain.Account;
import com.carddemo.batch.posttran.domain.CardXref;
import com.carddemo.batch.posttran.domain.TranCatBalance;
import com.carddemo.batch.posttran.domain.TranCatBalanceKey;
import com.carddemo.batch.posttran.domain.TransactionRecord;
import com.carddemo.batch.posttran.record.DailyTransaction;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import java.math.BigDecimal;
import java.time.Clock;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

/**
 * Business logic of CBTRN02C for one DALYTRAN record: 1500-VALIDATE-TRAN and
 * 2000-POST-TRANSACTION (2700-UPDATE-TCATBAL, 2800-UPDATE-ACCOUNT-REC, 2900-WRITE-TRANSACTION-FILE).
 * Keyed VSAM READ/WRITE/REWRITE become JPA find/persist/dirty-update in the caller's transaction.
 */
@Service
@Transactional(propagation = Propagation.MANDATORY)
public class PostingService {

    private static final int TCATBAL_FILLER_LENGTH = 22;

    @PersistenceContext
    private EntityManager em;

    private final Clock clock;
    private final Sysout sysout;

    /**
     * FILLER of the working-storage TRAN-CAT-BAL-RECORD. READ ... INTO overwrites it, but
     * INITIALIZE in 2700-A skips FILLER, so a newly created TCATBAL record inherits the filler of
     * the last TCATBAL record read in this run (spaces before the first successful read).
     */
    private String wsTranCatBalFiller = Pic.spaces(TCATBAL_FILLER_LENGTH);

    public PostingService(Clock clock, Sysout sysout) {
        this.clock = clock;
        this.sysout = sysout;
    }

    /** Working storage is fresh at program start. */
    @Transactional(propagation = Propagation.SUPPORTS)
    public void resetWorkingStorage() {
        wsTranCatBalFiller = Pic.spaces(TCATBAL_FILLER_LENGTH);
    }

    /**
     * 1500-VALIDATE-TRAN. Checks run in COBOL order; 102 and 103 are evaluated independently, so
     * when both fail 103 overwrites 102.
     */
    public ValidationResult validate(DailyTransaction tran) {
        // 1500-A-LOOKUP-XREF
        CardXref xref = em.find(CardXref.class, tran.cardNum());
        if (xref == null) {
            return new ValidationResult(ValidationResult.INVALID_CARD, "INVALID CARD NUMBER FOUND", null, null);
        }
        // 1500-B-LOOKUP-ACCT
        Account account = em.find(Account.class, xref.getAcctId());
        if (account == null) {
            return new ValidationResult(ValidationResult.ACCOUNT_NOT_FOUND, "ACCOUNT RECORD NOT FOUND", xref, null);
        }
        int reason = 0;
        String desc = "";
        // COMPUTE WS-TEMP-BAL (PIC S9(09)V99, no ON SIZE ERROR -> high-order truncation)
        BigDecimal wsTempBal = ZonedDecimal.truncate(
                account.getCurrCycCredit().subtract(account.getCurrCycDebit()).add(tran.amt()), 9, 2);
        if (account.getCreditLimit().compareTo(wsTempBal) < 0) {
            reason = ValidationResult.OVERLIMIT;
            desc = "OVERLIMIT TRANSACTION";
        }
        if (CobolCompare.compareAlphanumeric(account.getExpirationDate(), tran.origTs().substring(0, 10)) < 0) {
            reason = ValidationResult.AFTER_EXPIRATION;
            desc = "TRANSACTION RECEIVED AFTER ACCT EXPIRATION";
        }
        return new ValidationResult(reason, desc, xref, account);
    }

    /** 2000-POST-TRANSACTION. */
    public TransactionRecord post(DailyTransaction tran, ValidationResult validation) {
        TransactionRecord rec = new TransactionRecord(tran.id());
        rec.setTypeCd(tran.typeCd());
        rec.setCatCd(tran.catCd());
        rec.setSource(tran.source());
        rec.setDesc(tran.desc());
        rec.setAmt(tran.amt());
        rec.setMerchantId(tran.merchantId());
        rec.setMerchantName(tran.merchantName());
        rec.setMerchantCity(tran.merchantCity());
        rec.setMerchantZip(tran.merchantZip());
        rec.setCardNum(tran.cardNum());
        rec.setOrigTs(tran.origTs());
        rec.setProcTs(Db2Timestamp.now(clock));

        updateTranCatBalance(tran, validation.xref());
        updateAccount(tran, validation.account());
        writeTransaction(rec);
        return rec;
    }

    /** 2700-UPDATE-TCATBAL with 2700-A-CREATE-TCATBAL-REC / 2700-B-UPDATE-TCATBAL-REC. */
    void updateTranCatBalance(DailyTransaction tran, CardXref xref) {
        TranCatBalanceKey key = new TranCatBalanceKey(xref.getAcctId(), tran.typeCd(), tran.catCd());
        TranCatBalance existing = em.find(TranCatBalance.class, key);
        if (existing == null) {
            sysout.display("TCATBAL record not found for key : " + key.asRecordKey() + ".. Creating.");
            BigDecimal balance = ZonedDecimal.truncate(BigDecimal.ZERO.add(tran.amt()), 9, 2);
            em.persist(new TranCatBalance(key, balance, wsTranCatBalFiller));
        } else {
            wsTranCatBalFiller = existing.getFiller();
            existing.setBalance(ZonedDecimal.truncate(existing.getBalance().add(tran.amt()), 9, 2));
        }
    }

    /**
     * 2800-UPDATE-ACCOUNT-REC. Zero amounts count as credits. The REWRITE INVALID KEY branch
     * (reason 109) cannot fire because the record was just read; in the COBOL it is also a no-op
     * because the reason code is reset before the next record.
     */
    void updateAccount(DailyTransaction tran, Account account) {
        account.setCurrBal(ZonedDecimal.truncate(account.getCurrBal().add(tran.amt()), 10, 2));
        if (tran.amt().signum() >= 0) {
            account.setCurrCycCredit(ZonedDecimal.truncate(account.getCurrCycCredit().add(tran.amt()), 10, 2));
        } else {
            account.setCurrCycDebit(ZonedDecimal.truncate(account.getCurrCycDebit().add(tran.amt()), 10, 2));
        }
    }

    /** 2900-WRITE-TRANSACTION-FILE: a duplicate TRAN-ID is file status 22 and abends. */
    void writeTransaction(TransactionRecord rec) {
        if (em.find(TransactionRecord.class, rec.getTranId()) != null) {
            throw new Abend("ERROR WRITING TO TRANSACTION FILE", "22");
        }
        em.persist(rec);
    }
}
