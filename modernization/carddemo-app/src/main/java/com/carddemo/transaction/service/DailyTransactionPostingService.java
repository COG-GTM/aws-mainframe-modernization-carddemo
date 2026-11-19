package com.carddemo.transaction.service;

import com.carddemo.account.domain.Account;
import com.carddemo.account.repository.AccountRepository;
import com.carddemo.common.batch.SysoutWriter;
import com.carddemo.common.cobol.CobolDecimal;
import com.carddemo.common.record.Copybooks;
import com.carddemo.reference.domain.TranCatBalance;
import com.carddemo.reference.domain.TranCatBalanceId;
import com.carddemo.reference.repository.TranCatBalanceRepository;
import com.carddemo.transaction.domain.DailyTransaction;
import com.carddemo.transaction.domain.Transaction;
import com.carddemo.transaction.domain.TransactionRecords;
import com.carddemo.transaction.repository.TransactionRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DataAccessException;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.Optional;

/**
 * {@code CBTRN02C 2000-POST-TRANSACTION} (cbl:424-444): FR-POSTTRAN-07..10, PR-CBTRN02C-09..13. One valid DALYTRAN record
 * updates TCATBAL, then the account, then inserts the transaction, all in one database transaction (D-011, D-026).
 */
@Service
public class DailyTransactionPostingService {

    /** {@code TRAN-CAT-BAL PIC S9(09)V99} (CVTRA01Y). */
    static final int TCATBAL_INTEGER_DIGITS = 9;
    /** {@code ACCT-CURR-BAL}, {@code ACCT-CURR-CYC-CREDIT/DEBIT PIC S9(10)V99} (CVACT01Y). */
    static final int ACCOUNT_INTEGER_DIGITS = 10;
    /** VSAM FILE STATUS for a duplicate key on WRITE. */
    static final String STATUS_DUPLICATE_KEY = "22";
    /** VSAM FILE STATUS for a permanent I/O error; used for every other database failure (D-044). */
    static final String STATUS_IO_ERROR = "30";

    private static final Logger log = LoggerFactory.getLogger(DailyTransactionPostingService.class);

    private final TranCatBalanceRepository balances;
    private final AccountRepository accounts;
    private final TransactionRepository transactions;

    public DailyTransactionPostingService(TranCatBalanceRepository balances, AccountRepository accounts,
                                          TransactionRepository transactions) {
        this.balances = balances;
        this.accounts = accounts;
        this.transactions = transactions;
    }

    /**
     * Posts one validated record. {@code procTs} is {@code TRAN-PROC-TS} ({@code Z-GET-DB2-FORMAT-TIMESTAMP}); SYSOUT
     * lines go to {@code sysout}. Any failure writes the COBOL error lines and throws {@link PosttranAbendException};
     * the caller's transaction then rolls the whole record back.
     */
    @Transactional
    public void post(DailyTransaction transaction, long xrefAcctId, String procTs, SysoutWriter sysout) {
        updateTranCatBalance(transaction, xrefAcctId, sysout);
        updateAccount(transaction, xrefAcctId);
        writeTransaction(transaction, procTs, sysout);
    }

    /**
     * {@code 2700-UPDATE-TCATBAL} (cbl:467-543). Key = XREF-ACCT-ID + DALYTRAN-TYPE-CD + DALYTRAN-CAT-CD. Not found
     * (status 23): DISPLAY the creation line and WRITE a new record whose balance is {@code 0 + DALYTRAN-AMT}
     * ({@code INITIALIZE} then {@code ADD}); found: {@code ADD DALYTRAN-AMT} and REWRITE.
     */
    void updateTranCatBalance(DailyTransaction transaction, long xrefAcctId, SysoutWriter sysout) {
        TranCatBalanceId key = new TranCatBalanceId(xrefAcctId, transaction.typeCd().stripTrailing(), transaction.catCd());
        Optional<TranCatBalance> found;
        try {
            found = balances.findById(key);
        } catch (DataAccessException e) {
            throw abend(sysout, "ERROR READING TRANSACTION BALANCE FILE", STATUS_IO_ERROR, e);
        }
        if (found.isEmpty()) {
            sysout.println("TCATBAL record not found for key : " + tranCatKey(transaction, xrefAcctId) + ".. Creating.");
            TranCatBalance created = new TranCatBalance();
            created.setAcctId(xrefAcctId);
            created.setTypeCd(key.getTypeCd());
            created.setCatCd(key.getCatCd());
            CobolDecimal.Stored balance = CobolDecimal.storeWithSign(transaction.amount(), TCATBAL_INTEGER_DIGITS, 2);
            created.setBalance(balance.value());
            created.setBalanceNegativeZero(balance.negativeZero());
            try {
                balances.saveAndFlush(created);
            } catch (DataAccessException e) {
                throw abend(sysout, "ERROR WRITING TRANSACTION BALANCE FILE", status(e), e);
            }
        } else {
            TranCatBalance balance = found.get();
            CobolDecimal.Stored sum = CobolDecimal.storeWithSign(balance.getBalance().add(transaction.amount()),
                    TCATBAL_INTEGER_DIGITS, 2);
            balance.setBalance(sum.value());
            balance.setBalanceNegativeZero(sum.negativeZero());
            try {
                balances.saveAndFlush(balance);
            } catch (DataAccessException e) {
                throw abend(sysout, "ERROR REWRITING TRANSACTION BALANCE FILE", STATUS_IO_ERROR, e);
            }
        }
    }

    /**
     * {@code 2800-UPDATE-ACCOUNT-REC} (cbl:545-559): {@code CURR-BAL += AMT}; {@code AMT >= 0} adds to cycle credit, else
     * to cycle debit; REWRITE. REWRITE INVALID KEY only sets reason 109, which nothing reads afterwards (quirk Q-03): the
     * record is still posted, logged as a warning (D-027). A concurrent update (version conflict, B-04) abends.
     */
    void updateAccount(DailyTransaction transaction, long xrefAcctId) {
        Optional<Account> found = accounts.findById(xrefAcctId);
        if (found.isEmpty()) {
            log.warn("CBTRN02C reason 109 ACCOUNT RECORD NOT FOUND on account rewrite: acct {} tran {} card {}; posting "
                    + "continues (D-027)", xrefAcctId, transaction.id().stripTrailing(), maskCard(transaction.cardNum()));
            return;
        }
        Account account = found.get();
        BigDecimal amount = transaction.amount();
        CobolDecimal.Stored currBal = add(account.getCurrBal(), amount);
        account.setCurrBal(currBal.value());
        account.setCurrBalNegativeZero(currBal.negativeZero());
        // -0.00 >= 0 is true in COBOL too: a negative-zero amount goes to cycle credit
        if (amount.signum() >= 0) {
            CobolDecimal.Stored credit = add(account.getCurrCycCredit(), amount);
            account.setCurrCycCredit(credit.value());
            account.setCurrCycCreditNegativeZero(credit.negativeZero());
        } else {
            CobolDecimal.Stored debit = add(account.getCurrCycDebit(), amount);
            account.setCurrCycDebit(debit.value());
            account.setCurrCycDebitNegativeZero(debit.negativeZero());
        }
        try {
            accounts.saveAndFlush(account);
        } catch (DataAccessException e) {
            throw new PosttranAbendException("Account " + xrefAcctId + " rewrite failed", e);
        }
    }

    /**
     * {@code 2900-WRITE-TRANSACTION-FILE} (cbl:562-579). TRAN-RECORD is DALYTRAN-RECORD field for field (cbl:424-435;
     * CVTRA05Y and CVTRA06Y have the same layout) with {@code TRAN-PROC-TS} replaced; a MOVE keeps the sign of a -0.00 amount (D-052). A duplicate TRAN-ID (status 22) or
     * any other failure abends.
     */
    void writeTransaction(DailyTransaction transaction, String procTs, SysoutWriter sysout) {
        String record = Copybooks.CVTRA05Y.parse(transaction.record()).set("TRAN-PROC-TS", procTs).toString();
        Transaction posted = TransactionRecords.fromRecord(record);
        try {
            transactions.saveAndFlush(posted);
        } catch (DataAccessException e) {
            throw abend(sysout, "ERROR WRITING TO TRANSACTION FILE", status(e), e);
        }
    }

    /** {@code ADD DALYTRAN-AMT TO <account field>} into {@code PIC S9(10)V99}, sign kept as GnuCOBOL stores it (D-052). */
    private static CobolDecimal.Stored add(BigDecimal field, BigDecimal amount) {
        return CobolDecimal.storeWithSign(field.add(amount), ACCOUNT_INTEGER_DIGITS, 2);
    }

    /** {@code FD-TRAN-CAT-KEY}: {@code 9(11)} account + {@code X(02)} type + {@code 9(04)} category. */
    static String tranCatKey(DailyTransaction transaction, long xrefAcctId) {
        return String.format("%011d", xrefAcctId) + transaction.typeCd() + String.format("%04d", transaction.catCd());
    }

    private static String status(DataAccessException e) {
        return e instanceof DataIntegrityViolationException ? STATUS_DUPLICATE_KEY : STATUS_IO_ERROR;
    }

    private static PosttranAbendException abend(SysoutWriter sysout, String message, String status, Exception cause) {
        sysout.abend(message, status);
        return new PosttranAbendException(message + " (FILE STATUS " + status + ")", cause);
    }

    static String maskCard(String cardNum) {
        String trimmed = cardNum.stripTrailing();
        return trimmed.length() <= 4 ? trimmed : "*".repeat(trimmed.length() - 4) + trimmed.substring(trimmed.length() - 4);
    }
}
