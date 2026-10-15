package com.carddemo.account;

import com.carddemo.common.domain.Account;
import com.carddemo.common.domain.AccountRepository;
import com.carddemo.common.domain.CardXref;
import com.carddemo.common.domain.CardXrefRepository;
import com.carddemo.common.domain.Customer;
import com.carddemo.common.domain.CustomerRepository;
import com.carddemo.common.error.ApiException;
import jakarta.persistence.EntityManager;
import java.util.Objects;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/** COACTVWC (CAVW, account view) and COACTUPC (CAUP, account update). */
@Service
public class AccountService {

    private final AccountRepository accounts;
    private final CustomerRepository customers;
    private final CardXrefRepository xrefs;
    private final EntityManager em;

    public AccountService(AccountRepository accounts, CustomerRepository customers, CardXrefRepository xrefs,
                          EntityManager em) {
        this.accounts = accounts;
        this.customers = customers;
        this.xrefs = xrefs;
        this.em = em;
    }

    static long parseAccountId(String raw) {
        if (raw == null || raw.isBlank()) {
            throw ApiException.badRequest("ACCOUNT_REQUIRED", "Account number not provided");
        }
        if (!raw.matches("\\d{1,11}") || Long.parseLong(raw) == 0) {
            throw ApiException.badRequest("ACCOUNT_INVALID", "Account number must be a non zero 11 digit number");
        }
        return Long.parseLong(raw);
    }

    @Transactional(readOnly = true)
    public AccountDetails view(String rawAcctId) {
        long acctId = parseAccountId(rawAcctId);
        CardXref xref = xrefs.findFirstByAcctIdOrderByCardNumAsc(acctId).orElseThrow(() ->
                ApiException.notFound("XREF_NOT_FOUND", "Did not find this account in account card xref file"));
        Account account = accounts.findById(acctId).orElseThrow(() ->
                ApiException.notFound("ACCOUNT_NOT_FOUND", "Did not find this account in account master file"));
        Customer customer = customers.findById(xref.getCustId()).orElseThrow(() ->
                ApiException.notFound("CUSTOMER_NOT_FOUND", "Did not find associated customer in master file"));
        return AccountDetails.of(account, customer);
    }

    /**
     * COACTUPC: validate every edited field, refuse no-op updates, and detect concurrent changes. The legacy
     * program re-reads both records with READ UPDATE and compares them to the values it originally displayed;
     * here the client echoes the versions it was shown and JPA optimistic locking performs the same check.
     */
    @Transactional
    public String update(String rawAcctId, AccountUpdateRequest request) {
        long acctId = parseAccountId(rawAcctId);
        AccountUpdateValidator.validate(request);
        AccountDetails before = view(rawAcctId);
        Account account = accounts.findById(acctId).orElseThrow();
        Customer customer = customers.findById(before.custId()).orElseThrow();
        if (request.accountVersion() != account.getVersion() || request.customerVersion() != customer.getVersion()) {
            throw new OptimisticLockingFailureException("Record changed by some one else. Please review");
        }
        AccountUpdateRequest current = AccountUpdateRequest.from(before);
        if (Objects.equals(current.normalised(), request.normalised())) {
            throw ApiException.badRequest("NO_CHANGE", "No change detected with respect to values fetched.");
        }
        request.applyTo(account, customer);
        return "Changes committed to database";
    }
}
