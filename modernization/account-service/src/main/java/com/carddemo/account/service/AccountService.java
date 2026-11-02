package com.carddemo.account.service;

import com.carddemo.account.api.dto.AccountUpdateRequest;
import com.carddemo.account.domain.Account;
import com.carddemo.account.repository.AccountRepository;
import com.carddemo.common.error.BusinessRuleException;
import com.carddemo.common.error.NotFoundException;
import java.math.BigDecimal;
import java.time.LocalDate;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class AccountService {

    private final AccountRepository accounts;

    public AccountService(AccountRepository accounts) {
        this.accounts = accounts;
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
     * CBTRN02C paragraphs 1500-B-LOOKUP-ACCT (validation) and 2800-UPDATE-ACCOUNT-REC (update).
     *
     * <pre>
     * COMPUTE WS-TEMP-BAL = ACCT-CURR-CYC-CREDIT - ACCT-CURR-CYC-DEBIT + DALYTRAN-AMT
     * IF ACCT-CREDIT-LIMIT &lt; WS-TEMP-BAL            -&gt; reason 102 overlimit
     * IF ACCT-EXPIRAION-DATE &lt; DALYTRAN-ORIG-TS(1:10) -&gt; reason 103 expired
     * ADD DALYTRAN-AMT TO ACCT-CURR-BAL
     * IF DALYTRAN-AMT &gt;= 0 ADD TO ACCT-CURR-CYC-CREDIT ELSE ADD TO ACCT-CURR-CYC-DEBIT
     * </pre>
     *
     * The sign handling of the cycle buckets is kept exactly as in the COBOL: a negative amount is
     * added to (not subtracted from) the debit bucket.
     */
    @Transactional
    public PostingOutcome post(long acctId, BigDecimal amount, LocalDate transactionDate) {
        Account account = accounts.findById(acctId).orElse(null);
        if (account == null) {
            return new PostingOutcome(false, 101, "ACCOUNT RECORD NOT FOUND", null, null, null);
        }
        BigDecimal projected = account.cycleBalance().add(amount);
        if (account.getCreditLimit().compareTo(projected) < 0) {
            return PostingOutcome.rejected(account, 102, "OVERLIMIT TRANSACTION");
        }
        if (account.getExpirationDate() != null && account.getExpirationDate().isBefore(transactionDate)) {
            return PostingOutcome.rejected(account, 103, "TRANSACTION RECEIVED AFTER ACCT EXPIRATION");
        }
        account.setCurrBal(account.getCurrBal().add(amount));
        if (amount.signum() >= 0) {
            account.setCurrCycCredit(account.getCurrCycCredit().add(amount));
        } else {
            account.setCurrCycDebit(account.getCurrCycDebit().add(amount));
        }
        return PostingOutcome.posted(accounts.save(account));
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
