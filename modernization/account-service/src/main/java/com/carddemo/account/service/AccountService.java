package com.carddemo.account.service;

import com.carddemo.account.api.dto.AccountUpdateRequest;
import com.carddemo.account.domain.Account;
import com.carddemo.account.domain.AccountPosting;
import com.carddemo.account.repository.AccountPostingRepository;
import com.carddemo.account.repository.AccountRepository;
import com.carddemo.common.error.BusinessRuleException;
import com.carddemo.common.error.DuplicateKeyException;
import com.carddemo.common.error.NotFoundException;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class AccountService {

    private final AccountRepository accounts;
    private final AccountPostingRepository postings;

    public AccountService(AccountRepository accounts, AccountPostingRepository postings) {
        this.accounts = accounts;
        this.postings = postings;
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
     * COACTUPC: read for update, apply the screen edits, rewrite. The record level edits that the
     * COBOL performs after reading the stored record live here; the field level edits are bean
     * validation on {@link AccountUpdateRequest}.
     */
    @Transactional
    public Account update(long acctId, AccountUpdateRequest request) {
        Account account = get(acctId);
        if (request.expirationDate().isBefore(request.openDate())) {
            throw new BusinessRuleException("Expiration date cannot be before the open date");
        }
        if (request.reissueDate() != null && request.reissueDate().isBefore(request.openDate())) {
            throw new BusinessRuleException("Reissue date cannot be before the open date");
        }
        if (request.cashCreditLimit().compareTo(request.creditLimit()) > 0) {
            throw new BusinessRuleException("Cash credit limit cannot exceed the credit limit");
        }
        account.setActiveStatus(request.activeStatus().toUpperCase());
        account.setCreditLimit(request.creditLimit());
        account.setCashCreditLimit(request.cashCreditLimit());
        account.setOpenDate(request.openDate());
        account.setExpirationDate(request.expirationDate());
        account.setReissueDate(request.reissueDate());
        account.setAddrZip(request.addressZip());
        account.setGroupId(request.groupId());
        return accounts.save(account);
    }

    /**
     * Account side of CBTRN02C for one daily transaction: 1500-B-LOOKUP-ACCT (101, then 102 and
     * 103 both evaluated in that order, so 103 wins when both fail) and 2800-UPDATE-ACCOUNT-REC.
     * The posting is recorded under {@code tranId} in the same database transaction; a repeated call
     * with the same id returns the recorded outcome without touching the account again.
     */
    @Transactional
    public PostingOutcome post(long acctId, String tranId, BigDecimal amount, LocalDate transactionDate) {
        AccountPosting previous = postings.findById(tranId).orElse(null);
        if (previous != null) {
            return replay(previous, acctId, amount, transactionDate);
        }
        Account account = accounts.findById(acctId).orElse(null);
        if (account == null) {
            return new PostingOutcome(false, 101, "ACCOUNT RECORD NOT FOUND", null, null, null);
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
        Account saved = accounts.save(account);
        postings.save(new AccountPosting(tranId, saved, amount, transactionDate, LocalDateTime.now()));
        return PostingOutcome.posted(saved);
    }

    private static PostingOutcome replay(AccountPosting previous, long acctId, BigDecimal amount,
                                         LocalDate transactionDate) {
        if (previous.getAcctId() != acctId || previous.getAmount().compareTo(amount) != 0
                || !previous.getTransactionDate().equals(transactionDate)) {
            throw new DuplicateKeyException("Transaction " + previous.getTranId()
                    + " was already posted to account " + previous.getAcctId() + " with amount "
                    + previous.getAmount() + " on " + previous.getTransactionDate());
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
