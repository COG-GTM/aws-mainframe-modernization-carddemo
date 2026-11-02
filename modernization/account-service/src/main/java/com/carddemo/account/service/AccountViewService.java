package com.carddemo.account.service;

import com.carddemo.account.api.dto.AccountUpdateRequest;
import com.carddemo.account.api.dto.AccountViewResponse;
import com.carddemo.account.client.CardXrefClient;
import com.carddemo.account.client.CardXrefView;
import com.carddemo.account.client.CustomerClient;
import com.carddemo.account.client.CustomerUpdate;
import com.carddemo.account.client.CustomerView;
import com.carddemo.account.domain.Account;
import com.carddemo.account.repository.AccountRepository;
import com.carddemo.account.validation.AccountUpdateValidator;
import com.carddemo.account.validation.CobolEdits;
import com.carddemo.common.api.ApiError.FieldProblem;
import com.carddemo.common.cobol.CobolValues;
import com.carddemo.common.error.BusinessRuleException;
import com.carddemo.common.error.FieldValidationException;
import com.carddemo.common.error.NotFoundException;
import com.carddemo.common.error.RecordChangedException;
import java.time.Clock;
import java.time.LocalDate;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * COACTVWC (account view) and COACTUPC (account update): account id → CXACAIX cross reference
 * (card-service) → ACCTDAT (here) → CUSTDAT (customer-service).
 */
@Service
public class AccountViewService {

    /** COACTVWC 2210-EDIT-ACCOUNT (two blanks after "must", as in the COBOL literal). */
    public static final String VIEW_ACCOUNT_ID_INVALID = "Account Filter must  be a non-zero 11 digit number";
    /** COACTUPC 1210-EDIT-ACCOUNT. */
    public static final String UPDATE_ACCOUNT_ID_INVALID =
            "Account Number if supplied must be a 11 digit Non-Zero Number";
    /** COACTUPC 88 NO-CHANGES-DETECTED. */
    public static final String NO_CHANGES_DETECTED = "No change detected with respect to values fetched.";

    private final AccountRepository accounts;
    private final CardXrefClient xrefs;
    private final CustomerClient customers;
    private final Clock clock;

    @Autowired
    public AccountViewService(AccountRepository accounts, CardXrefClient xrefs, CustomerClient customers) {
        this(accounts, xrefs, customers, Clock.systemDefaultZone());
    }

    AccountViewService(AccountRepository accounts, CardXrefClient xrefs, CustomerClient customers, Clock clock) {
        this.accounts = accounts;
        this.xrefs = xrefs;
        this.customers = customers;
        this.clock = clock;
    }

    /** COACTVWC 2210-EDIT-ACCOUNT then 9000-READ-ACCT. */
    @Transactional(readOnly = true)
    public AccountViewResponse view(String accountId) {
        Records records = read(parseAccountId(accountId, VIEW_ACCOUNT_ID_INVALID));
        return AccountViewResponse.from(records.account(), records.customer());
    }

    /**
     * COACTUPC, the PF5 path: 1205-COMPARE-OLD-NEW, 1200-EDIT-MAP-INPUTS, then 9600-WRITE-PROCESSING
     * (READ UPDATE both records, 9700-CHECK-CHANGE-IN-REC, REWRITE ACCTDAT, REWRITE CUSTDAT, rollback
     * of the account when the customer rewrite fails). The COBOL compares against the copy it read for
     * display; here the request's versions say which copy that was. When they still match, "no change"
     * is checked first, as in the COBOL; the conflict itself is reported after the edits, as 9600 runs
     * after them.
     */
    @Transactional
    public AccountViewResponse update(String accountId, AccountUpdateRequest request) {
        Records records = read(parseAccountId(accountId, UPDATE_ACCOUNT_ID_INVALID));
        Account account = records.account();
        CustomerView customer = records.customer();
        boolean sameVersions = request.accountVersion() == account.getVersion()
                && request.customerVersion() == customer.version();
        if (sameVersions && AccountChangeDetector.unchanged(request, account, customer)) {
            throw new BusinessRuleException(NO_CHANGES_DETECTED);
        }
        List<FieldProblem> problems =
                AccountUpdateValidator.validate(request, customer.countryCode(), LocalDate.now(clock));
        if (!problems.isEmpty()) {
            throw new FieldValidationException(problems);
        }
        if (!sameVersions) {
            throw new RecordChangedException();
        }
        apply(request, account);
        accounts.saveAndFlush(account);
        CustomerView updated = customers.update(customer.customerId(), customerUpdate(request, customer));
        return AccountViewResponse.from(account, updated);
    }

    static long parseAccountId(String raw, String message) {
        String value = raw == null ? "" : raw.strip();
        if (!value.matches("\\d{1,11}") || Long.parseLong(value) == 0) {
            throw new FieldValidationException(List.of(new FieldProblem("accountId", message)));
        }
        return Long.parseLong(value);
    }

    /** 9200-GETCARDXREF-BYACCT, 9300-GETACCTDATA-BYACCT, 9400-GETCUSTDATA-BYCUST, in that order. */
    private Records read(long accountId) {
        String id = CobolValues.zeroPad(accountId, 11);
        CardXrefView xref = xrefs.byAccount(accountId)
                .orElseThrow(() -> new NotFoundException("Account:" + id + " not found in Cross ref file."));
        Account account = accounts.findById(accountId)
                .orElseThrow(() -> new NotFoundException("Account:" + id + " not found in Acct Master file."));
        CustomerView customer = customers.get(xref.customerId())
                .orElseThrow(() -> new NotFoundException("CustId:" + CobolValues.zeroPad(xref.customerId(), 9)
                        + " not found in customer master."));
        return new Records(account, customer);
    }

    /**
     * The ACCT-UPDATE-RECORD fields. ACCT-ADDR-ZIP is not part of the update: COACTUPC's
     * ACCT-UPDATE-RECORD has no such field, so the COBOL REWRITE lands ACCT-UPDATE-GROUP-ID on the
     * ACCT-ADDR-ZIP bytes and blanks ACCT-GROUP-ID; Java writes the group id to its own column and
     * keeps the zip.
     */
    private static void apply(AccountUpdateRequest r, Account account) {
        account.setActiveStatus(CobolEdits.normalize(r.activeStatus()));
        account.setCurrBal(CobolEdits.numvalC(r.currentBalance()));
        account.setCreditLimit(CobolEdits.numvalC(r.creditLimit()));
        account.setCashCreditLimit(CobolEdits.numvalC(r.cashCreditLimit()));
        account.setOpenDate(CobolEdits.toDate(r.openDate()));
        account.setExpirationDate(CobolEdits.toDate(r.expirationDate()));
        account.setReissueDate(CobolEdits.toDate(r.reissueDate()));
        account.setCurrCycCredit(CobolEdits.numvalC(r.currentCycleCredit()));
        account.setCurrCycDebit(CobolEdits.numvalC(r.currentCycleDebit()));
        account.setGroupId(CobolEdits.normalize(r.groupId()));
    }

    /** The CUST-UPDATE-RECORD fields; country is the protected (stored) value. */
    private static CustomerUpdate customerUpdate(AccountUpdateRequest r, CustomerView stored) {
        return new CustomerUpdate(
                CobolEdits.normalize(r.firstName()),
                CobolEdits.normalize(r.middleName()),
                CobolEdits.normalize(r.lastName()),
                CobolEdits.normalize(r.addressLine1()),
                CobolEdits.normalize(r.addressLine2()),
                CobolEdits.normalize(r.city()),
                CobolEdits.normalize(r.stateCode()),
                stored.countryCode(),
                CobolEdits.normalize(r.zip()),
                CobolEdits.toStoredPhone(r.phoneNumber1()),
                CobolEdits.toStoredPhone(r.phoneNumber2()),
                CobolEdits.normalize(r.ssn()) == null ? null : CobolEdits.toSsn(r.ssn()),
                CobolEdits.normalize(r.governmentIssuedId()),
                CobolEdits.toDate(r.dateOfBirth()),
                CobolEdits.normalize(r.eftAccountId()),
                CobolEdits.normalize(r.primaryCardHolder()),
                Integer.parseInt(CobolEdits.normalize(r.ficoScore())),
                stored.version());
    }

    private record Records(Account account, CustomerView customer) {
    }
}
