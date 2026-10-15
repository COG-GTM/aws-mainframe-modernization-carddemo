package com.carddemo.account;

import com.carddemo.common.error.ApiException;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Set;

/** Field edits from COACTUPC paragraphs 1200-EDIT-MAP-INPUTS onward, in screen order, first error wins. */
final class AccountUpdateValidator {

    /** CSLKPCDY state table (US states, DC and territories). */
    static final Set<String> US_STATES = Set.of("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI",
            "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV",
            "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT",
            "VA", "WA", "WV", "WI", "WY", "DC", "AS", "GU", "MP", "PR", "VI");

    private AccountUpdateValidator() {
    }

    static void validate(AccountUpdateRequest r) {
        yesNo(r.activeStatus(), "Account Status");
        amount(r.creditLimit(), "Credit Limit");
        date(r.expirationDate(), "Expiry Date");
        amount(r.cashCreditLimit(), "Cash Credit Limit");
        date(r.reissueDate(), "Reissue Date");
        amount(r.currBal(), "Current Balance");
        amount(r.currCycCredit(), "Current Cycle Credit Limit");
        amount(r.currCycDebit(), "Current Cycle Debit Limit");
        date(r.openDate(), "Open Date");
        ssn(r.ssn());
        date(r.dob(), "Date of Birth");
        if (r.dob() != null && r.dob().isAfter(LocalDate.now())) {
            fail("Date of Birth: cannot be in the future");
        }
        if (r.ficoCreditScore() == null) {
            fail("FICO Score must be supplied.");
        }
        if (r.ficoCreditScore() < 300 || r.ficoCreditScore() > 850) {
            fail("FICO Score: should be between 300 and 850");
        }
        alpha(r.firstName(), "First Name", true);
        alpha(r.middleName(), "Middle Name", false);
        alpha(r.lastName(), "Last Name", true);
        required(r.addrLine1(), "Address Line 1");
        required(r.addrStateCd(), "State");
        alpha(r.addrStateCd(), "State", true);
        if (!US_STATES.contains(r.addrStateCd().toUpperCase())) {
            fail("State: is not a valid state code");
        }
        required(r.addrZip(), "Zip");
        if (!r.addrZip().trim().matches("\\d{5}")) {
            fail("Zip must be all numeric.");
        }
        required(r.addrLine3(), "City");
        alpha(r.addrLine3(), "City", true);
        required(r.addrCountryCd(), "Country");
        alpha(r.addrCountryCd(), "Country", true);
        phone(r.phoneNum1(), "Phone Number 1", true);
        phone(r.phoneNum2(), "Phone Number 2", false);
        required(r.eftAccountId(), "EFT Account Id");
        if (!r.eftAccountId().trim().matches("\\d{1,10}")) {
            fail("EFT Account Id must be all numeric.");
        }
        yesNo(r.priCardHolderInd(), "Primary Card Holder");
    }

    private static void required(String v, String name) {
        if (v == null || v.isBlank()) {
            fail(name + " must be supplied.");
        }
    }

    private static void yesNo(String v, String name) {
        required(v, name);
        if (!v.equalsIgnoreCase("Y") && !v.equalsIgnoreCase("N")) {
            fail(name + " must be Y or N.");
        }
    }

    private static void amount(BigDecimal v, String name) {
        if (v == null) {
            fail(name + " must be supplied.");
        }
        if (v.scale() > 2 || v.abs().compareTo(new BigDecimal("9999999999.99")) > 0) {
            fail(name + " is not valid");
        }
    }

    private static void date(LocalDate v, String name) {
        if (v == null) {
            fail(name + " must be supplied.");
        }
    }

    private static void alpha(String v, String name, boolean mandatory) {
        if (v == null || v.isBlank()) {
            if (mandatory) {
                fail(name + " must be supplied.");
            }
            return;
        }
        if (!v.trim().matches("[A-Za-z ]+")) {
            fail(name + " can have alphabets only.");
        }
    }

    /** US phone: (999)999-9999, area code not starting 0/1 (CSLKPCDY NANP edit). */
    private static void phone(String v, String name, boolean mandatory) {
        if (v == null || v.isBlank()) {
            if (mandatory) {
                fail(name + " must be supplied.");
            }
            return;
        }
        if (!v.trim().matches("\\(\\d{3}\\)\\d{3}-\\d{4}")) {
            fail(name + " is not valid");
        }
        if (v.charAt(1) == '0' || v.charAt(1) == '1') {
            fail(name + ": Not valid area code");
        }
    }

    /** SSN edit: 3-2-4 digits; part 1 not 000, 666 or 900-999; part 2 not 00; part 3 not 0000. */
    private static void ssn(String v) {
        required(v, "SSN");
        String digits = v.replace("-", "");
        if (!digits.matches("\\d{9}")) {
            fail("SSN: First 3 chars must be all numeric.");
        }
        int part1 = Integer.parseInt(digits.substring(0, 3));
        if (part1 == 0 || part1 == 666 || part1 >= 900) {
            fail("SSN: First 3 chars: should not be 000, 666, or between 900 and 999");
        }
        if (digits.substring(3, 5).equals("00")) {
            fail("SSN 4th & 5th chars must not be zero.");
        }
        if (digits.substring(5).equals("0000")) {
            fail("SSN Last 4 chars must not be zero.");
        }
    }

    private static void fail(String message) {
        throw ApiException.badRequest("VALIDATION", message);
    }
}
