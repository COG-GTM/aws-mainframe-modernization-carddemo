package com.carddemo.account.online;

import com.carddemo.account.Account;
import com.carddemo.account.AccountStatus;
import com.carddemo.customer.Customer;
import com.carddemo.customer.PrimaryCardHolder;
import java.math.BigDecimal;

/**
 * The editable fields of map {@code CACTUPA} as text, the shape of {@code ACUP-NEW-DETAILS} (what was typed) and
 * {@code ACUP-OLD-DETAILS} (what was fetched). Customer id and country are protected on the map and not part of it.
 */
public record AccountChanges(
        String activeStatus,
        DateParts openDate,
        String creditLimit,
        DateParts expiryDate,
        String cashCreditLimit,
        DateParts reissueDate,
        String currentBalance,
        String currentCycleCredit,
        String currentCycleDebit,
        String groupId,
        SsnParts ssn,
        DateParts dateOfBirth,
        String ficoScore,
        String firstName,
        String middleName,
        String lastName,
        String addressLine1,
        String addressLine2,
        String state,
        String zip,
        String city,
        PhoneParts phone1,
        PhoneParts phone2,
        String governmentId,
        String eftAccountId,
        String primaryCardHolder) {

    /** {@code OPNYEAR}/{@code OPNMON}/{@code OPNDAY} style split date (4/2/2). */
    public record DateParts(String year, String month, String day) {

        public DateParts {
            year = field(year, 4);
            month = field(month, 2);
            day = field(day, 2);
        }

        static DateParts of(String yyyyMmDd) {
            String d = Fields.fit(yyyyMmDd, 10);
            return new DateParts(d.substring(0, 4), d.substring(5, 7), d.substring(8, 10));
        }

        /** {@code WS-EDIT-DATE-CCYYMMDD}. */
        String ccyymmdd() {
            return Fields.fit(year, 4) + Fields.fit(month, 2) + Fields.fit(day, 2);
        }
    }

    /** {@code ACTSSN1}/{@code ACTSSN2}/{@code ACTSSN3} (3/2/4). */
    public record SsnParts(String part1, String part2, String part3) {

        public SsnParts {
            part1 = field(part1, 3);
            part2 = field(part2, 2);
            part3 = field(part3, 4);
        }

        static SsnParts of(int ssn) {
            String s = String.format("%09d", ssn);
            return new SsnParts(s.substring(0, 3), s.substring(3, 5), s.substring(5, 9));
        }
    }

    /** {@code ACSPHnA}/{@code ACSPHnB}/{@code ACSPHnC}: area code, prefix, line number (3/3/4). */
    public record PhoneParts(String areaCode, String prefix, String lineNumber) {

        public PhoneParts {
            areaCode = field(areaCode, 3);
            prefix = field(prefix, 3);
            lineNumber = field(lineNumber, 4);
        }

        /** {@code CUST-PHONE-NUM} is stored {@code (AAA)BBB-CCCC}. */
        static PhoneParts of(String stored) {
            String p = Fields.fit(stored, 15);
            return new PhoneParts(p.substring(1, 4), p.substring(5, 8), p.substring(9, 13));
        }

        boolean blank() {
            return areaCode.isEmpty() && prefix.isEmpty() && lineNumber.isEmpty();
        }

        /** The {@code (AAA)BBB-CCCC} form {@code 9600-WRITE-PROCESSING} writes; blank when no part is given. */
        String formatted() {
            return blank() ? "" : "(" + Fields.fit(areaCode, 3) + ")" + Fields.fit(prefix, 3) + "-"
                    + Fields.fit(lineNumber, 4);
        }
    }

    public AccountChanges {
        activeStatus = field(activeStatus, 1);
        openDate = openDate == null ? new DateParts(null, null, null) : openDate;
        creditLimit = field(creditLimit, 15);
        expiryDate = expiryDate == null ? new DateParts(null, null, null) : expiryDate;
        cashCreditLimit = field(cashCreditLimit, 15);
        reissueDate = reissueDate == null ? new DateParts(null, null, null) : reissueDate;
        currentBalance = field(currentBalance, 15);
        currentCycleCredit = field(currentCycleCredit, 15);
        currentCycleDebit = field(currentCycleDebit, 15);
        groupId = field(groupId, 10);
        ssn = ssn == null ? new SsnParts(null, null, null) : ssn;
        dateOfBirth = dateOfBirth == null ? new DateParts(null, null, null) : dateOfBirth;
        ficoScore = field(ficoScore, 3);
        firstName = field(firstName, 25);
        middleName = field(middleName, 25);
        lastName = field(lastName, 25);
        addressLine1 = field(addressLine1, 50);
        addressLine2 = field(addressLine2, 50);
        state = field(state, 2);
        zip = field(zip, 5);
        city = field(city, 50);
        phone1 = phone1 == null ? new PhoneParts(null, null, null) : phone1;
        phone2 = phone2 == null ? new PhoneParts(null, null, null) : phone2;
        governmentId = field(governmentId, 20);
        eftAccountId = field(eftAccountId, 10);
        primaryCardHolder = field(primaryCardHolder, 1);
    }

    /** {@code 9500-STORE-FETCHED-DATA}: the record as the map shows it before any change. */
    public static AccountChanges fetched(Account account, Customer customer) {
        return new AccountChanges(
                account.getActiveStatus() == null ? "" : account.getActiveStatus().code(),
                DateParts.of(account.getOpenDate()),
                amount(account.getCreditLimit()),
                DateParts.of(account.getExpirationDate()),
                amount(account.getCashCreditLimit()),
                DateParts.of(account.getReissueDate()),
                amount(account.getCurrBal()),
                amount(account.getCurrCycCredit()),
                amount(account.getCurrCycDebit()),
                account.getGroupId(),
                SsnParts.of(customer.getSsn()),
                DateParts.of(customer.getDob()),
                String.format("%03d", customer.getFicoCreditScore()),
                customer.getFirstName(),
                customer.getMiddleName(),
                customer.getLastName(),
                customer.getAddrLine1(),
                customer.getAddrLine2(),
                customer.getAddrStateCd(),
                customer.getAddrZip(),
                customer.getAddrLine3(),
                PhoneParts.of(customer.getPhoneNum1()),
                PhoneParts.of(customer.getPhoneNum2()),
                customer.getGovtIssuedId(),
                customer.getEftAccountId(),
                customer.getPriCardHolderInd() == null ? "" : customer.getPriCardHolderInd().code());
    }

    static AccountStatus status(String code) {
        return AccountStatus.fromCode(code);
    }

    static PrimaryCardHolder primaryCardHolder(String code) {
        return PrimaryCardHolder.fromCode(code);
    }

    private static String amount(BigDecimal value) {
        return value == null ? "" : value.setScale(2).toPlainString();
    }

    /**
     * {@code 1100-RECEIVE-MAP}: {@code *} or spaces is {@code LOW-VALUES} (blank, held as the empty string); otherwise
     * the typed text without trailing spaces, cut to the BMS field length.
     */
    private static String field(String typed, int length) {
        if (typed == null || typed.isBlank() || typed.stripTrailing().equals("*")) {
            return "";
        }
        String value = typed.length() > length ? typed.substring(0, length) : typed;
        return Fields.rightTrim(value);
    }
}
