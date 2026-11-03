package com.carddemo.account.api.dto;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

/**
 * The unprotected input fields of the COACTUP map (CACTUPA), account and customer half, plus the
 * record versions read with {@code GET /api/v1/accounts/{accountId}/view}; the versions replace the
 * {@code ACUP-OLD-*} copy that COACTUPC keeps in its COMMAREA and compares in 9700-CHECK-CHANGE-IN-REC.
 *
 * <p>Values are text, as typed on the screen, so the COBOL edits ({@code AccountUpdateValidator}) can
 * report "must be supplied" / "is not valid" with the COBOL wording; JSON numbers are accepted for the
 * amount and numeric fields too. The {@code @Size} limits are the BMS field lengths. Multi-part
 * screen fields are one string: dates {@code YYYY-MM-DD}, SSN {@code 999-99-9999}, phone
 * {@code 999-999-9999} or the stored {@code (999)999-9999}.
 *
 * @param ssn optional: the view returns the SSN masked, so omitting it keeps the stored SSN and skips
 *            1265-EDIT-US-SSN (deviation: COACTUPC always edits and rewrites it)
 */
public record AccountUpdateRequest(
        @NotNull Long accountVersion,
        @NotNull Long customerVersion,
        @Size(max = 1) String activeStatus,
        @Size(max = 15) String creditLimit,
        @Size(max = 15) String cashCreditLimit,
        @Size(max = 15) String currentBalance,
        @Size(max = 15) String currentCycleCredit,
        @Size(max = 15) String currentCycleDebit,
        @Size(max = 10) String openDate,
        @Size(max = 10) String expirationDate,
        @Size(max = 10) String reissueDate,
        @Size(max = 10) String groupId,
        @Size(max = 11) String ssn,
        @Size(max = 10) String dateOfBirth,
        @Size(max = 3) String ficoScore,
        @Size(max = 25) String firstName,
        @Size(max = 25) String middleName,
        @Size(max = 25) String lastName,
        @Size(max = 50) String addressLine1,
        @Size(max = 50) String addressLine2,
        @Size(max = 50) String city,
        @Size(max = 2) String stateCode,
        @Size(max = 5) String zip,
        @Size(max = 15) String phoneNumber1,
        @Size(max = 15) String phoneNumber2,
        @Size(max = 20) String governmentIssuedId,
        @Size(max = 10) String eftAccountId,
        @Size(max = 1) String primaryCardHolder) {
}
