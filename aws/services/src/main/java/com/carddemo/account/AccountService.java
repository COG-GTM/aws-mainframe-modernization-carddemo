package com.carddemo.account;

import com.carddemo.account.AccountDtos.AccountUpdateRequest;
import com.carddemo.account.AccountDtos.AccountView;
import com.carddemo.account.AccountDtos.CustomerUpdateRequest;
import com.carddemo.account.AccountDtos.CustomerView;
import com.carddemo.account.AccountRepository.XrefRow;
import com.carddemo.common.ApiException;
import com.carddemo.common.CobolEdits;
import com.carddemo.common.ErrorCode;
import com.carddemo.common.LegacyMessages;
import com.carddemo.common.Money;
import com.carddemo.common.Text;
import com.carddemo.common.UsLookups;
import com.carddemo.common.ValidationErrors;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Objects;
import org.springframework.dao.PessimisticLockingFailureException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/** COACTVWC (view) and COACTUPC (update) of an account with its customer. */
@Service
public class AccountService {

    static final String VIEW_PROGRAM = "COACTVWC";
    static final String UPDATE_PROGRAM = "COACTUPC";

    private final AccountRepository repository;
    private final UsLookups lookups;

    public AccountService(AccountRepository repository, UsLookups lookups) {
        this.repository = repository;
        this.lookups = lookups;
    }

    /** 1210-EDIT-ACCOUNT of COACTVWC/COACTUPC. */
    public static long parseAccountId(String value, String program) {
        if (Text.isBlank(value)) {
            throw ApiException.validation(program, "acctId", "Account number not provided");
        }
        String v = value.strip();
        if (v.length() > 11 || !Text.isDigits(v) || Text.isAllZero(v)) {
            throw ApiException.validation(program, "acctId", "Account number must be a non zero 11 digit number");
        }
        return Long.parseLong(v);
    }

    @Transactional(readOnly = true)
    public AccountView view(String acctIdIn) {
        return load(parseAccountId(acctIdIn, VIEW_PROGRAM), VIEW_PROGRAM).view();
    }

    @Transactional
    public AccountView update(String acctIdIn, AccountUpdateRequest request) {
        long acctId = parseAccountId(acctIdIn, UPDATE_PROGRAM);
        if (request == null || request.customer() == null) {
            throw new ApiException(ErrorCode.INVALID_REQUEST, "No input received", UPDATE_PROGRAM);
        }
        if (request.version() == null || request.customer().version() == null) {
            throw ApiException.validation(UPDATE_PROGRAM, "version", "version is required");
        }
        Loaded current = load(acctId, UPDATE_PROGRAM);
        if (!changed(current, request)) {
            throw ApiException.businessRule(UPDATE_PROGRAM, LegacyMessages.NO_CHANGE);
        }
        Validated input = validate(request);

        AccountRecord lockedAccount;
        CustomerRecord lockedCustomer;
        try {
            lockedAccount = repository.lockAccountNoWait(acctId).orElseThrow(
                    () -> new ApiException(ErrorCode.LOCKED, "Could not lock account record for update",
                            UPDATE_PROGRAM));
        } catch (PessimisticLockingFailureException ex) {
            throw new ApiException(ErrorCode.LOCKED, "Could not lock account record for update", UPDATE_PROGRAM);
        }
        try {
            lockedCustomer = repository.lockCustomerNoWait(current.customer().custId()).orElseThrow(
                    () -> new ApiException(ErrorCode.LOCKED, "Could not lock customer record for update",
                            UPDATE_PROGRAM));
        } catch (PessimisticLockingFailureException ex) {
            throw new ApiException(ErrorCode.LOCKED, "Could not lock customer record for update", UPDATE_PROGRAM);
        }
        if (lockedAccount.version() != request.version()
                || lockedCustomer.version() != request.customer().version()) {
            throw ApiException.concurrentUpdate(UPDATE_PROGRAM);
        }

        CustomerUpdateRequest c = request.customer();
        AccountRecord newAccount = new AccountRecord(acctId, input.activeStatus, input.currBal, input.creditLimit,
                input.cashCreditLimit, input.openDate, input.expirationDate, input.reissueDate, input.currCycCredit,
                input.currCycDebit,
                request.addrZip() == null ? lockedAccount.addrZip() : Text.trim(request.addrZip()),
                request.groupId() == null ? lockedAccount.groupId() : Text.trim(request.groupId()),
                lockedAccount.version());
        CustomerRecord newCustomer = new CustomerRecord(lockedCustomer.custId(), c.firstName().strip(),
                Text.trimToEmpty(c.middleName()), c.lastName().strip(), c.addrLine1().strip(),
                Text.trimToEmpty(c.addrLine2()), c.addrLine3().strip(), Text.upperTrim(c.addrStateCd()),
                Text.upperTrim(c.addrCountryCd()), c.addrZip().strip(), input.phone1, input.phone2, input.ssn,
                c.govtIssuedId() == null ? lockedCustomer.govtIssuedId() : Text.trim(c.govtIssuedId()), input.dob,
                c.eftAccountId().strip(), Text.upperTrim(c.priCardHolderInd()), input.fico,
                lockedCustomer.version());
        if (repository.updateAccount(newAccount) != 1 || repository.updateCustomer(newCustomer) != 1) {
            throw new ApiException(ErrorCode.INTERNAL_ERROR, LegacyMessages.UPDATE_FAILED, UPDATE_PROGRAM);
        }
        return load(acctId, UPDATE_PROGRAM).view().withMessage(LegacyMessages.CHANGES_COMMITTED);
    }

    private Loaded load(long acctId, String program) {
        XrefRow xref = repository.findFirstXrefByAccount(acctId).orElseThrow(
                () -> ApiException.notFound(program, "Did not find this account in account card xref file"));
        AccountRecord account = repository.findAccount(acctId).orElseThrow(
                () -> ApiException.notFound(program, "Did not find this account in account master file"));
        CustomerRecord customer = repository.findCustomer(xref.custId()).orElseThrow(
                () -> ApiException.notFound(program, "Did not find associated customer in master file"));
        return new Loaded(account, customer, repository);
    }

    /** 1205-COMPARE-OLD-NEW: case-insensitive, trimmed comparison of every editable field. */
    private static boolean changed(Loaded current, AccountUpdateRequest r) {
        AccountRecord a = current.account();
        CustomerRecord c = current.customer();
        CustomerUpdateRequest n = r.customer();
        return !sameText(a.activeStatus(), r.activeStatus())
                || !sameAmount(a.currBal(), r.currBal())
                || !sameAmount(a.creditLimit(), r.creditLimit())
                || !sameAmount(a.cashCreditLimit(), r.cashCreditLimit())
                || !sameText(dateText(a.openDate()), r.openDate())
                || !sameText(dateText(a.expirationDate()), r.expirationDate())
                || !sameText(dateText(a.reissueDate()), r.reissueDate())
                || !sameAmount(a.currCycCredit(), r.currCycCredit())
                || !sameAmount(a.currCycDebit(), r.currCycDebit())
                || (r.groupId() != null && !sameText(a.groupId(), r.groupId()))
                || (r.addrZip() != null && !sameText(a.addrZip(), r.addrZip()))
                || !sameText(c.ssn(), Text.trimToEmpty(n.ssn()).replace("-", ""))
                || !sameText(dateText(c.dob()), n.dob())
                || !sameText(c.ficoCreditScore() == null ? "" : String.valueOf(c.ficoCreditScore()),
                        n.ficoCreditScore())
                || !sameText(c.firstName(), n.firstName())
                || !sameText(c.middleName(), n.middleName())
                || !sameText(c.lastName(), n.lastName())
                || !sameText(c.addrLine1(), n.addrLine1())
                || !sameText(c.addrLine2(), n.addrLine2())
                || !sameText(c.addrLine3(), n.addrLine3())
                || !sameText(c.addrStateCd(), n.addrStateCd())
                || !sameText(c.addrCountryCd(), n.addrCountryCd())
                || !sameText(c.addrZip(), n.addrZip())
                || !sameText(c.phoneNum1(), n.phoneNum1())
                || !sameText(c.phoneNum2(), n.phoneNum2())
                || (n.govtIssuedId() != null && !sameText(c.govtIssuedId(), n.govtIssuedId()))
                || !sameText(c.eftAccountId(), n.eftAccountId())
                || !sameText(c.priCardHolderInd(), n.priCardHolderInd());
    }

    /** 1200-EDIT-MAP-INPUTS: edits in legacy order. */
    private Validated validate(AccountUpdateRequest r) {
        ValidationErrors errors = new ValidationErrors(UPDATE_PROGRAM);
        CobolEdits edit = new CobolEdits(errors);
        CustomerUpdateRequest c = r.customer();
        Validated v = new Validated();

        edit.yesNo("activeStatus", "Account Status", Text.upperTrim(r.activeStatus()));
        v.activeStatus = Text.upperTrim(r.activeStatus());
        v.openDate = edit.date("openDate", "Open Date", r.openDate());
        v.creditLimit = edit.signedAmount("creditLimit", "Credit Limit", r.creditLimit());
        v.expirationDate = edit.date("expirationDate", "Expiry Date", r.expirationDate());
        v.cashCreditLimit = edit.signedAmount("cashCreditLimit", "Cash Credit Limit", r.cashCreditLimit());
        v.reissueDate = edit.date("reissueDate", "Reissue Date", r.reissueDate());
        v.currBal = edit.signedAmount("currBal", "Current Balance", r.currBal());
        v.currCycCredit = edit.signedAmount("currCycCredit", "Current Cycle Credit Limit", r.currCycCredit());
        v.currCycDebit = edit.signedAmount("currCycDebit", "Current Cycle Debit Limit", r.currCycDebit());
        v.ssn = edit.usSsn("customer.ssn", c.ssn());
        v.dob = edit.dateOfBirth("customer.dob", "Date of Birth", c.dob());
        v.fico = edit.ficoScore("customer.ficoCreditScore", c.ficoCreditScore());
        edit.alphaRequired("customer.firstName", "First Name", c.firstName(), 25);
        edit.alphaOptional("customer.middleName", "Middle Name", c.middleName(), 25);
        edit.alphaRequired("customer.lastName", "Last Name", c.lastName(), 25);
        edit.mandatory("customer.addrLine1", "Address Line 1", c.addrLine1(), 50);
        edit.maxLength("customer.addrLine2", "Address Line 2", c.addrLine2(), 50);
        boolean stateOk = edit.usState("customer.addrStateCd", c.addrStateCd(), lookups);
        boolean zipOk = edit.numericRequired("customer.addrZip", "Zip", c.addrZip(), 5);
        edit.alphaRequired("customer.addrLine3", "City", c.addrLine3(), 50);
        edit.alphaRequired("customer.addrCountryCd", "Country", c.addrCountryCd(), 3);
        v.phone1 = edit.usPhone("customer.phoneNum1", "Phone Number 1", c.phoneNum1(), lookups);
        v.phone2 = edit.usPhone("customer.phoneNum2", "Phone Number 2", c.phoneNum2(), lookups);
        edit.numericRequired("customer.eftAccountId", "EFT Account Id", c.eftAccountId(), 10);
        edit.yesNo("customer.priCardHolderInd", "Primary Card Holder", Text.upperTrim(c.priCardHolderInd()));
        if (stateOk && zipOk && !lookups.isValidStateZip(Text.upperTrim(c.addrStateCd()), c.addrZip().strip())) {
            errors.add("customer.addrZip", "Invalid zip code for state");
        }
        edit.maxLength("customer.govtIssuedId", "Government Issued Id", c.govtIssuedId(), 20);
        edit.maxLength("groupId", "Group Id", r.groupId(), 10);
        edit.maxLength("addrZip", "Account Zip", r.addrZip(), 10);
        errors.throwIfAny();
        return v;
    }

    private static boolean sameText(String oldValue, String newValue) {
        return Text.upperTrim(oldValue).equals(Text.upperTrim(newValue));
    }

    private static boolean sameAmount(BigDecimal oldValue, String newValue) {
        BigDecimal parsed = CobolEdits.parseNumvalC(newValue);
        if (parsed == null || oldValue == null) {
            return oldValue == null && Text.isBlank(newValue);
        }
        return oldValue.compareTo(parsed) == 0;
    }

    private static String dateText(LocalDate date) {
        return date == null ? "" : date.toString();
    }

    private static final class Validated {
        String activeStatus;
        BigDecimal currBal;
        BigDecimal creditLimit;
        BigDecimal cashCreditLimit;
        LocalDate openDate;
        LocalDate expirationDate;
        LocalDate reissueDate;
        BigDecimal currCycCredit;
        BigDecimal currCycDebit;
        String ssn;
        LocalDate dob;
        Integer fico;
        String phone1;
        String phone2;
    }

    private record Loaded(AccountRecord account, CustomerRecord customer, AccountRepository repository) {

        AccountView view() {
            AccountRecord a = account;
            CustomerRecord c = customer;
            CustomerView cv = new CustomerView(c.custId(), c.firstName(), c.middleName(), c.lastName(),
                    c.addrLine1(), c.addrLine2(), c.addrLine3(), c.addrStateCd(), c.addrCountryCd(), c.addrZip(),
                    c.phoneNum1(), c.phoneNum2(), c.ssn(), c.govtIssuedId(), dateText(c.dob()), c.eftAccountId(),
                    c.priCardHolderInd(), c.ficoCreditScore(), c.version());
            return new AccountView(a.acctId(), a.activeStatus(), Money.format(a.currBal()),
                    Money.format(a.creditLimit()), Money.format(a.cashCreditLimit()), dateText(a.openDate()),
                    dateText(a.expirationDate()), dateText(a.reissueDate()), Money.format(a.currCycCredit()),
                    Money.format(a.currCycDebit()), Objects.toString(a.groupId(), ""),
                    Objects.toString(a.addrZip(), ""), a.version(), cv, repository.findCardsByAccount(a.acctId()),
                    null);
        }
    }
}
