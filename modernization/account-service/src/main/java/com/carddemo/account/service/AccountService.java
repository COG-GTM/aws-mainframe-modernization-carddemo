package com.carddemo.account.service;

import com.carddemo.account.domain.Account;
import com.carddemo.account.domain.AccountPosting;
import com.carddemo.account.domain.PostingType;
import com.carddemo.account.repository.AccountPostingRepository;
import com.carddemo.account.repository.AccountRepository;
import com.carddemo.common.error.DuplicateKeyException;
import com.carddemo.common.error.NotFoundException;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionTemplate;

@Service
public class AccountService {

    private final AccountRepository accounts;
    private final AccountPostingRepository postings;
    private final TransactionTemplate transaction;

    public AccountService(AccountRepository accounts, AccountPostingRepository postings,
                          PlatformTransactionManager transactionManager) {
        this.accounts = accounts;
        this.postings = postings;
        this.transaction = new TransactionTemplate(transactionManager);
    }

    /** COACTVWC: read ACCTDATA by account id. */
    @Transactional(readOnly = true)
    public Account get(long acctId) {
        return accounts.findById(acctId)
                .orElseThrow(() -> new NotFoundException("Account " + acctId + " not found"));
    }

    @Transactional(readOnly = true)
    public Page<Account> list(Pageable pageable) {
        return accounts.findAll(pageable);
    }

    /**
     * Account side of CBTRN02C for one daily transaction: 1500-B-LOOKUP-ACCT (101, then 102 and
     * 103 both evaluated in that order, so 103 wins when both fail) and 2800-UPDATE-ACCOUNT-REC.
     * The posting is recorded under {@code tranId} in the same database transaction; a repeated call
     * with the same id returns the recorded outcome without touching the account again. When a
     * concurrent request with the same id commits first, this one fails on the account version or the
     * ledger key, rolls back, and answers from the ledger row the winner wrote.
     */
    public PostingOutcome post(long acctId, String tranId, BigDecimal amount, LocalDate transactionDate) {
        return post(acctId, tranId, amount, transactionDate, PostingType.DAILY_TRANSACTION);
    }

    /** As {@link #post(long, String, BigDecimal, LocalDate)}, following the paragraph {@code type} names. */
    public PostingOutcome post(long acctId, String tranId, BigDecimal amount, LocalDate transactionDate,
                               PostingType type) {
        try {
            return transaction.execute(status -> postOnce(acctId, tranId, amount, transactionDate, type));
        } catch (DataIntegrityViolationException | OptimisticLockingFailureException conflict) {
            AccountPosting winner = postings.findById(tranId).orElseThrow(() -> conflict);
            return replay(winner, acctId, amount, transactionDate, type);
        }
    }

    private PostingOutcome postOnce(long acctId, String tranId, BigDecimal amount, LocalDate transactionDate,
                                    PostingType type) {
        AccountPosting previous = postings.findById(tranId).orElse(null);
        if (previous != null) {
            return replay(previous, acctId, amount, transactionDate, type);
        }
        Account account = accounts.findById(acctId).orElse(null);
        if (account == null) {
            return new PostingOutcome(false, 101, "ACCOUNT RECORD NOT FOUND", null, null, null);
        }
        if (type == PostingType.BILL_PAYMENT) {
            account.setCurrBal(account.getCurrBal().add(amount));
            Account saved = accounts.saveAndFlush(account);
            postings.saveAndFlush(new AccountPosting(tranId, saved, amount, transactionDate, LocalDateTime.now(), type));
            return PostingOutcome.posted(saved);
        }
        Integer reasonCode = null;
        String reasonDescription = null;
        // COMPUTE WS-TEMP-BAL = ACCT-CURR-CYC-CREDIT - ACCT-CURR-CYC-DEBIT + DALYTRAN-AMT
        BigDecimal projected = account.cycleBalance().add(amount);
        if (account.getCreditLimit().compareTo(projected) < 0) {
            reasonCode = 102;
            reasonDescription = "OVERLIMIT TRANSACTION";
        }
        // IF ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS (1:10): a blank expiration date compares low.
        if (account.getExpirationDate() == null || account.getExpirationDate().isBefore(transactionDate)) {
            reasonCode = 103;
            reasonDescription = "TRANSACTION RECEIVED AFTER ACCT EXPIRATION";
        }
        if (reasonCode != null) {
            return PostingOutcome.rejected(account, reasonCode, reasonDescription);
        }
        account.setCurrBal(account.getCurrBal().add(amount));
        if (amount.signum() >= 0) {
            account.setCurrCycCredit(account.getCurrCycCredit().add(amount));
        } else {
            account.setCurrCycDebit(account.getCurrCycDebit().add(amount));
        }
        Account saved = accounts.saveAndFlush(account);
        postings.saveAndFlush(new AccountPosting(tranId, saved, amount, transactionDate, LocalDateTime.now(), type));
        return PostingOutcome.posted(saved);
    }

    private static PostingOutcome replay(AccountPosting previous, long acctId, BigDecimal amount,
                                         LocalDate transactionDate, PostingType type) {
        if (previous.getAcctId() != acctId || previous.getAmount().compareTo(amount) != 0
                || !previous.getTransactionDate().equals(transactionDate) || previous.getPostingType() != type) {
            throw new DuplicateKeyException("Transaction " + previous.getTranId()
                    + " was already posted to account " + previous.getAcctId() + " with amount "
                    + previous.getAmount() + " on " + previous.getTransactionDate() + " as "
                    + previous.getPostingType());
        }
        return new PostingOutcome(true, null, null, previous.getCurrBal(), previous.getCurrCycCredit(),
                previous.getCurrCycDebit());
    }

    /** CBACT04C paragraph 1050-UPDATE-ACCOUNT, run once per account at the cycle break. */
    @Transactional
    public Account settleInterest(long acctId, BigDecimal totalInterest) {
        Account account = get(acctId);
        account.setCurrBal(account.getCurrBal().add(totalInterest));
        account.setCurrCycCredit(BigDecimal.ZERO);
        account.setCurrCycDebit(BigDecimal.ZERO);
        return accounts.save(account);
    }
}
