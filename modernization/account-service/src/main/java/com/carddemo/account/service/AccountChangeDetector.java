package com.carddemo.account.service;

import com.carddemo.account.api.dto.AccountUpdateRequest;
import com.carddemo.account.client.CustomerView;
import com.carddemo.account.domain.Account;
import com.carddemo.account.validation.CobolEdits;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.Locale;
import java.util.Objects;

/**
 * COACTUPC 1205-COMPARE-OLD-NEW: the request against the records it was built from. Text fields that
 * the COBOL compares with {@code FUNCTION UPPER-CASE (FUNCTION TRIM (...))} ignore case and padding;
 * amounts compare by value; dates, phone parts, EFT id and FICO score compare as typed text.
 */
final class AccountChangeDetector {

    private AccountChangeDetector() {
    }

    static boolean unchanged(AccountUpdateRequest r, Account a, CustomerView c) {
        return accountUnchanged(r, a) && customerUnchanged(r, c);
    }

    private static boolean accountUnchanged(AccountUpdateRequest r, Account a) {
        return sameIgnoreCase(r.activeStatus(), a.getActiveStatus())
                && sameAmount(r.currentBalance(), a.getCurrBal())
                && sameAmount(r.creditLimit(), a.getCreditLimit())
                && sameAmount(r.cashCreditLimit(), a.getCashCreditLimit())
                && sameDate(r.openDate(), a.getOpenDate())
                && sameDate(r.expirationDate(), a.getExpirationDate())
                && sameDate(r.reissueDate(), a.getReissueDate())
                && sameAmount(r.currentCycleCredit(), a.getCurrCycCredit())
                && sameAmount(r.currentCycleDebit(), a.getCurrCycDebit())
                && sameIgnoreCase(r.groupId(), a.getGroupId());
    }

    /** A supplied SSN counts as a change: the stored one is only known masked (see AccountUpdateRequest#ssn). */
    private static boolean customerUnchanged(AccountUpdateRequest r, CustomerView c) {
        return sameIgnoreCase(r.firstName(), c.firstName())
                && sameIgnoreCase(r.middleName(), c.middleName())
                && sameIgnoreCase(r.lastName(), c.lastName())
                && sameIgnoreCase(r.addressLine1(), c.addressLine1())
                && sameIgnoreCase(r.addressLine2(), c.addressLine2())
                && sameIgnoreCase(r.city(), c.addressLine3())
                && sameIgnoreCase(r.stateCode(), c.stateCode())
                && sameIgnoreCase(r.zip(), c.zip())
                && Arrays.equals(CobolEdits.phoneParts(r.phoneNumber1()), CobolEdits.phoneParts(c.phone1()))
                && Arrays.equals(CobolEdits.phoneParts(r.phoneNumber2()), CobolEdits.phoneParts(c.phone2()))
                && CobolEdits.normalize(r.ssn()) == null
                && sameIgnoreCase(r.governmentIssuedId(), c.governmentIssuedId())
                && sameDate(r.dateOfBirth(), c.dateOfBirth())
                && Objects.equals(CobolEdits.normalize(r.eftAccountId()), CobolEdits.normalize(c.eftAccountId()))
                && sameIgnoreCase(r.primaryCardHolder(), c.primaryCardHolderIndicator())
                && Objects.equals(CobolEdits.normalize(r.ficoScore()),
                        c.ficoScore() == null ? null : "%03d".formatted(c.ficoScore()));
    }

    private static boolean sameIgnoreCase(String typed, String stored) {
        String a = CobolEdits.normalize(typed);
        String b = CobolEdits.normalize(stored);
        return a == null ? b == null : b != null && a.toUpperCase(Locale.ROOT).equals(b.toUpperCase(Locale.ROOT));
    }

    private static boolean sameAmount(String typed, BigDecimal stored) {
        BigDecimal value = CobolEdits.numvalC(typed);
        return value != null && stored != null && value.compareTo(stored) == 0;
    }

    private static boolean sameDate(String typed, LocalDate stored) {
        String value = CobolEdits.normalize(typed);
        return value == null ? stored == null : stored != null && value.equals(stored.toString());
    }
}
