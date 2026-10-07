package com.carddemo.account.online;

import com.carddemo.account.Account;
import com.carddemo.account.AccountRecord;
import com.carddemo.account.AccountRepository;
import com.carddemo.common.AbendException;
import com.carddemo.common.FieldEditException;
import com.carddemo.common.Versions;
import com.carddemo.customer.Customer;
import com.carddemo.customer.CustomerRecord;
import com.carddemo.customer.CustomerRepository;
import java.time.Clock;
import java.time.LocalDate;
import java.util.List;
import org.springframework.dao.DataAccessException;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * {@code COACTUPC} as one request: {@code 9000-READ-ACCT}, the version check that replaces {@code 9700-CHECK-CHANGE-IN-REC}
 * (ADR-0010), {@code 1205-COMPARE-OLD-NEW}, the {@code 1200} field edits, then either "validated" (state {@code N},
 * waiting for PF5) or, when confirmed, the {@code 9600-WRITE-PROCESSING} REWRITE of account and customer in one
 * transaction.
 */
@Service
public class AccountUpdateService {

    public static final String MSG_UPDATE_FAILED = "Update of record failed";

    private final AccountLookup lookup;
    private final AccountRepository accounts;
    private final CustomerRepository customers;
    private final Clock clock;

    public AccountUpdateService(AccountLookup lookup, AccountRepository accounts, CustomerRepository customers,
            Clock clock) {
        this.lookup = lookup;
        this.accounts = accounts;
        this.customers = customers;
        this.clock = clock;
    }

    /** {@code ACUP-CHANGE-ACTION} after the request, with the {@code 3250-SETUP-INFOMSG} text. */
    public enum State {
        /** {@code S}: no change detected; nothing written. */
        SHOW("Update account details presented above."),
        /** {@code N}: changes validated, not confirmed (PF5 not pressed); nothing written. */
        VALIDATED("Changes validated.Press F5 to save"),
        /** {@code C}: account and customer rewritten. */
        COMMITTED("Changes committed to database");

        private final String infoMessage;

        State(String infoMessage) {
            this.infoMessage = infoMessage;
        }

        public String infoMessage() {
            return infoMessage;
        }
    }

    /** @param message {@code WS-RETURN-MSG}: the no-change message, else empty */
    public record Outcome(State state, String message, AccountDetails details) {
    }

    @Transactional
    public Outcome update(long acctId, AccountChanges typed, long accountVersion, long customerVersion,
            boolean confirm) {
        AccountDetails details = lookup.find(acctId);
        Account account = details.account();
        Customer customer = details.customer();
        Versions.requireCurrent(Account.class, acctId, accountVersion, account.getVersion());
        Versions.requireCurrent(Customer.class, customer.getCustId(), customerVersion, customer.getVersion());

        if (AccountUpdateEdits.noChanges(AccountChanges.fetched(account, customer), typed)) {
            return new Outcome(State.SHOW, AccountUpdateEdits.MSG_NO_CHANGES, details);
        }
        List<AccountUpdateEdits.FieldError> errors = AccountUpdateEdits.edit(typed, customer.getAddrCountryCd(),
                LocalDate.now(clock));
        if (!errors.isEmpty()) {
            throw new FieldEditException(errors.get(0).field(), errors.get(0).message(),
                    errors.stream().map(AccountUpdateEdits.FieldError::field).distinct().toList());
        }
        if (!confirm) {
            return new Outcome(State.VALIDATED, "", details);
        }
        account.update(accountRecord(account, typed));
        customer.update(customerRecord(customer, typed));
        try {
            accounts.saveAndFlush(account);
            customers.saveAndFlush(customer);
        } catch (OptimisticLockingFailureException e) {
            throw e;
        } catch (DataAccessException e) {
            throw new AbendException(AbendException.CARDDEMO_ABEND_CODE, MSG_UPDATE_FAILED, e);
        }
        return new Outcome(State.COMMITTED, "", details);
    }

    /** {@code ACCT-UPDATE-RECORD}; {@code ACCT-ADDR-ZIP} is not on the map and keeps its stored value. */
    private static AccountRecord accountRecord(Account account, AccountChanges typed) {
        return new AccountRecord(account.getAcctId(), AccountChanges.status(typed.activeStatus()),
                amount(typed.currentBalance()), amount(typed.creditLimit()), amount(typed.cashCreditLimit()),
                isoDate(typed.openDate()), isoDate(typed.expiryDate()), isoDate(typed.reissueDate()),
                amount(typed.currentCycleCredit()), amount(typed.currentCycleDebit()), account.getAddrZip(),
                typed.groupId());
    }

    /** {@code CUST-UPDATE-RECORD}; the country ({@code ACSCTRY}) is protected and keeps its stored value. */
    private static CustomerRecord customerRecord(Customer customer, AccountChanges typed) {
        AccountChanges.SsnParts ssn = typed.ssn();
        return new CustomerRecord(customer.getCustId(), typed.firstName(), typed.middleName(), typed.lastName(),
                typed.addressLine1(), typed.addressLine2(), typed.city(), typed.state(),
                customer.getAddrCountryCd(), typed.zip(), typed.phone1().formatted(), typed.phone2().formatted(),
                Integer.parseInt(ssn.part1() + ssn.part2() + ssn.part3()), typed.governmentId(),
                isoDate(typed.dateOfBirth()), typed.eftAccountId(),
                AccountChanges.primaryCardHolder(typed.primaryCardHolder()), Integer.parseInt(typed.ficoScore()));
    }

    private static java.math.BigDecimal amount(String text) {
        return AccountUpdateEdits.amount(text).orElseThrow();
    }

    /**
     * {@code YYYY-MM-DD} from the edited parts. The day is written with two digits ({@code CSUTLDPY} accepts
     * {@code " 5"}, which the COBOL would store as typed) so the stored text stays a date.
     */
    private static String isoDate(AccountChanges.DateParts date) {
        return date.year() + "-" + date.month() + "-" + String.format("%02d", Integer.parseInt(date.day().strip()));
    }
}
