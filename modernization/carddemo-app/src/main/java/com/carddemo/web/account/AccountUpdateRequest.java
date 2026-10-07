package com.carddemo.web.account;

import com.carddemo.account.online.AccountChanges;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

/**
 * The unprotected fields of map {@code CACTUPA} as typed (text, edited by the server like {@code COACTUPC}), plus the
 * versions of the account and customer that were displayed (ADR-0010) and the PF5 confirmation.
 */
@Schema(description = "Editable CACTUPA fields as text (the server applies the COACTUPC edits), the versions that "
        + "were displayed and the PF5 confirmation. Blank or '*' means not supplied.")
public record AccountUpdateRequest(
        @Schema(description = "ACCOUNT version from the GET (optimistic lock)", example = "0")
        @NotNull(message = "accountVersion must be supplied.") Long accountVersion,
        @Schema(description = "CUSTOMER version from the GET (optimistic lock)", example = "0")
        @NotNull(message = "customerVersion must be supplied.") Long customerVersion,
        @Schema(description = "false = ENTER (validate only, state N 'Changes validated.Press F5 to save'); true = "
                + "PF5 (validate and save in one unit of work)", example = "false", defaultValue = "false")
        Boolean confirm,
        @Schema(description = "ACSTTUS: Y or N", example = "Y") @Size(max = 1) String activeStatus,
        @Schema(description = "OPNYEAR/OPNMON/OPNDAY") @Valid DateInput openDate,
        @Schema(description = "ACRDLIM, signed amount (NUMVAL-C format)", example = "20200.00")
        @Size(max = 15) String creditLimit,
        @Schema(description = "EXPYEAR/EXPMON/EXPDAY") @Valid DateInput expiryDate,
        @Schema(description = "ACSHLIM", example = "10200.00") @Size(max = 15) String cashCreditLimit,
        @Schema(description = "RISYEAR/RISMON/RISDAY") @Valid DateInput reissueDate,
        @Schema(description = "ACURBAL", example = "1940.00") @Size(max = 15) String currentBalance,
        @Schema(description = "ACRCYCR", example = "0.00") @Size(max = 15) String currentCycleCredit,
        @Schema(description = "ACRCYDB", example = "0.00") @Size(max = 15) String currentCycleDebit,
        @Schema(description = "AADDGRP (not edited, written as typed)", example = "A000000000")
        @Size(max = 10) String groupId,
        @Schema(description = "ACTSSN1/ACTSSN2/ACTSSN3") @Valid SsnInput ssn,
        @Schema(description = "DOBYEAR/DOBMON/DOBDAY") @Valid DateInput dateOfBirth,
        @Schema(description = "ACSTFCO, 300..850", example = "274") @Size(max = 3) String ficoScore,
        @Schema(description = "ACSFNAM", example = "Immanuel") @Size(max = 25) String firstName,
        @Schema(description = "ACSMNAM", example = "Madeline") @Size(max = 25) String middleName,
        @Schema(description = "ACSLNAM", example = "Kessler") @Size(max = 25) String lastName,
        @Schema(description = "ACSADL1", example = "618 Deshaun Route") @Size(max = 50) String addressLine1,
        @Schema(description = "ACSADL2 (not edited, written as typed)", example = "Apt. 802")
        @Size(max = 50) String addressLine2,
        @Schema(description = "ACSSTTE, a CSLKPCDY US state code", example = "NC") @Size(max = 2) String state,
        @Schema(description = "ACSZIPC, 5 digits matching the state", example = "27601") @Size(max = 5) String zip,
        @Schema(description = "ACSCITY (address line 3)", example = "Altenwerthshire") @Size(max = 50) String city,
        @Schema(description = "ACSPH1A/B/C") @Valid PhoneInput phone1,
        @Schema(description = "ACSPH2A/B/C") @Valid PhoneInput phone2,
        @Schema(description = "ACSGOVT (not edited, written as typed)", example = "00000000000049368437")
        @Size(max = 20) String governmentId,
        @Schema(description = "ACSEFTC, 10 digits", example = "0053581756") @Size(max = 10) String eftAccountId,
        @Schema(description = "ACSPFLG: Y or N", example = "Y") @Size(max = 1) String primaryCardHolder) {

    @Schema(description = "Date split like the map: year (4), month (2), day (2)")
    public record DateInput(
            @Schema(example = "2014") @Size(max = 4) String year,
            @Schema(example = "11") @Size(max = 2) String month,
            @Schema(example = "20") @Size(max = 2) String day) {

        AccountChanges.DateParts toParts() {
            return new AccountChanges.DateParts(year, month, day);
        }

        static DateInput of(AccountChanges.DateParts parts) {
            return new DateInput(parts.year(), parts.month(), parts.day());
        }
    }

    @Schema(description = "SSN split like the map: 3 / 2 / 4 digits")
    public record SsnInput(
            @Schema(example = "020") @Size(max = 3) String part1,
            @Schema(example = "97") @Size(max = 2) String part2,
            @Schema(example = "3888") @Size(max = 4) String part3) {

        static SsnInput of(AccountChanges.SsnParts parts) {
            return new SsnInput(parts.part1(), parts.part2(), parts.part3());
        }
    }

    @Schema(description = "US phone split like the map: area code (3), prefix (3), line number (4); all blank = none")
    public record PhoneInput(
            @Schema(example = "908") @Size(max = 3) String areaCode,
            @Schema(example = "119") @Size(max = 3) String prefix,
            @Schema(example = "8310") @Size(max = 4) String lineNumber) {

        AccountChanges.PhoneParts toParts() {
            return new AccountChanges.PhoneParts(areaCode, prefix, lineNumber);
        }

        static PhoneInput of(AccountChanges.PhoneParts parts) {
            return new PhoneInput(parts.areaCode(), parts.prefix(), parts.lineNumber());
        }
    }

    boolean confirmed() {
        return Boolean.TRUE.equals(confirm);
    }

    AccountChanges toChanges() {
        return new AccountChanges(activeStatus, date(openDate), creditLimit, date(expiryDate), cashCreditLimit,
                date(reissueDate), currentBalance, currentCycleCredit, currentCycleDebit, groupId,
                ssn == null ? null : new AccountChanges.SsnParts(ssn.part1(), ssn.part2(), ssn.part3()),
                date(dateOfBirth), ficoScore, firstName, middleName, lastName, addressLine1, addressLine2, state, zip,
                city, phone(phone1), phone(phone2), governmentId, eftAccountId, primaryCardHolder);
    }

    /** The form {@code COACTUPC} shows after a fetch ({@code 3202-SHOW-ORIGINAL-VALUES}), not confirmed. */
    static AccountUpdateRequest prefilled(AccountChanges values, long accountVersion, long customerVersion) {
        return new AccountUpdateRequest(accountVersion, customerVersion, false, values.activeStatus(),
                DateInput.of(values.openDate()), values.creditLimit(), DateInput.of(values.expiryDate()),
                values.cashCreditLimit(), DateInput.of(values.reissueDate()), values.currentBalance(),
                values.currentCycleCredit(), values.currentCycleDebit(), values.groupId(), SsnInput.of(values.ssn()),
                DateInput.of(values.dateOfBirth()), values.ficoScore(), values.firstName(), values.middleName(),
                values.lastName(), values.addressLine1(), values.addressLine2(), values.state(), values.zip(),
                values.city(), PhoneInput.of(values.phone1()), PhoneInput.of(values.phone2()), values.governmentId(),
                values.eftAccountId(), values.primaryCardHolder());
    }

    private static AccountChanges.DateParts date(DateInput input) {
        return input == null ? null : input.toParts();
    }

    private static AccountChanges.PhoneParts phone(PhoneInput input) {
        return input == null ? null : input.toParts();
    }
}
