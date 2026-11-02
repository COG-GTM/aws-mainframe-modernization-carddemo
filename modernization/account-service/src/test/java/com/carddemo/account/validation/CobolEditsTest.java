package com.carddemo.account.validation;

import static org.assertj.core.api.Assertions.assertThat;

import java.math.BigDecimal;
import java.time.LocalDate;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class CobolEditsTest {

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {
            "2000.00|2000.00", "1,000.5|1000.50", "$1,234.56|1234.56", "-12.34|-12.34", "12.34-|-12.34",
            "12.34CR|-12.34", "12.34 db|-12.34", "+5|5.00", ".5|0.50", "1.239|1.23", "9999999999.99|9999999999.99"})
    void numvalCAcceptsWhatTestNumvalCAccepts(String typed, String expected) {
        assertThat(CobolEdits.numvalC(typed)).isEqualByComparingTo(new BigDecimal(expected));
    }

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {"abc", "-", "1.2.3", "--5", "-5-", "12a", "99999999999", "1 2"})
    void numvalCRejectsInvalidOrOversizedAmounts(String typed) {
        assertThat(CobolEdits.numvalC(typed)).isNull();
    }

    @Test
    void signedAmountUsesTheCobolMessages() {
        assertThat(CobolEdits.signedAmount("Credit Limit", " ")).isEqualTo("Credit Limit must be supplied.");
        assertThat(CobolEdits.signedAmount("Credit Limit", "*")).isEqualTo("Credit Limit must be supplied.");
        assertThat(CobolEdits.signedAmount("Credit Limit", "x1")).isEqualTo("Credit Limit is not valid");
        assertThat(CobolEdits.signedAmount("Credit Limit", "-1.00")).isNull();
    }

    @Test
    void yesNoIsCaseSensitiveAndTreatsZeroAsBlank() {
        assertThat(CobolEdits.yesNo("Account Status", "Y")).isNull();
        assertThat(CobolEdits.yesNo("Account Status", "y")).isEqualTo("Account Status must be Y or N.");
        assertThat(CobolEdits.yesNo("Account Status", "0")).isEqualTo("Account Status must be supplied.");
        assertThat(CobolEdits.yesNo("Account Status", null)).isEqualTo("Account Status must be supplied.");
    }

    @ParameterizedTest
    @CsvSource(delimiter = '|', nullValues = "NULL", value = {
            "NULL|Open Date : Year must be supplied.",
            "-01-15|Open Date : Year must be supplied.",
            "99-01-15|Open Date must be 4 digit number.",
            "1899-01-15|Open Date : Century is not valid.",
            "2020--15|Open Date : Month must be supplied.",
            "2020-13-15|Open Date: Month must be a number between 1 and 12.",
            "2020-00-15|Open Date: Month must be a number between 1 and 12.",
            "2020-01|Open Date : Day must be supplied.",
            "2020-01-32|Open Date:day must be a number between 1 and 31.",
            "2020-04-31|Open Date:Cannot have 31 days in this month.",
            "2020-02-30|Open Date:Cannot have 30 days in this month.",
            "2021-02-29|Open Date:Not a leap year.Cannot have 29 days in this month.",
            "1900-02-29|Open Date:Not a leap year.Cannot have 29 days in this month."})
    void dateEditsFollowCsutldpy(String typed, String message) {
        assertThat(CobolEdits.date("Open Date", typed)).isEqualTo(message);
    }

    @ParameterizedTest
    @CsvSource({"2020-02-29", "2000-02-29", "1999-12-31", "2020-1-5"})
    void validDatesPass(String typed) {
        assertThat(CobolEdits.date("Open Date", typed)).isNull();
    }

    @Test
    void theYearMessageWinsOverLaterParts() {
        assertThat(CobolEdits.date("Open Date", "x-13-40")).isEqualTo("Open Date must be 4 digit number.");
    }

    @Test
    void dateOfBirthMustBeBeforeToday() {
        LocalDate today = LocalDate.of(2026, 11, 2);
        assertThat(CobolEdits.dateOfBirth("Date of Birth", "2026-11-02", today))
                .isEqualTo("Date of Birth:cannot be in the future");
        assertThat(CobolEdits.dateOfBirth("Date of Birth", "2026-11-01", today)).isNull();
    }

    @ParameterizedTest
    @CsvSource(delimiter = '|', nullValues = "NULL", value = {
            "NULL|NULL", "--|NULL", "--12|NULL", "908-119-8310|NULL", "(908)119-8310|NULL",
            "-119-8310|Phone Number 1: Area code must be supplied.",
            "9x8-119-8310|Phone Number 1: Area code must be A 3 digit number.",
            "000-119-8310|Phone Number 1: Area code cannot be zero",
            "211-119-8310|Phone Number 1: Not valid North America general purpose area code",
            "908--8310|Phone Number 1: Prefix code must be supplied.",
            "908-11-8310|Phone Number 1: Prefix code must be A 3 digit number.",
            "908-000-8310|Phone Number 1: Prefix code cannot be zero",
            "908-119|Phone Number 1: Line number code must be supplied.",
            "908-119-831|Phone Number 1: Line number code must be A 4 digit number.",
            "908-119-0000|Phone Number 1: Line number code cannot be zero"})
    void phoneEditsFollow1260(String typed, String message) {
        assertThat(CobolEdits.usPhone("Phone Number 1", typed)).isEqualTo(message);
    }

    @Test
    void storedPhoneFormatIsParenthesised() {
        assertThat(CobolEdits.toStoredPhone("908-119-8310")).isEqualTo("(908)119-8310");
        assertThat(CobolEdits.toStoredPhone("(908)119-8310")).isEqualTo("(908)119-8310");
        assertThat(CobolEdits.toStoredPhone("--12")).isEqualTo("(   )   -  12");
        assertThat(CobolEdits.toStoredPhone(" ")).isNull();
    }

    @ParameterizedTest
    @CsvSource(delimiter = '|', nullValues = "NULL", value = {
            "123-45-6789|NULL", "123456789|NULL",
            "-45-6789|SSN: First 3 chars must be supplied.",
            "12a-45-6789|SSN: First 3 chars must be all numeric.",
            "000-45-6789|SSN: First 3 chars must not be zero.",
            "666-45-6789|SSN: First 3 chars: should not be 000, 666, or between 900 and 999",
            "901-45-6789|SSN: First 3 chars: should not be 000, 666, or between 900 and 999",
            "123-00-6789|SSN 4th & 5th chars must not be zero.",
            "123-4-6789|SSN 4th & 5th chars must be all numeric.",
            "123-45|SSN Last 4 chars must be supplied."})
    void ssnEditsFollow1265(String typed, String message) {
        assertThat(CobolEdits.usSsn(typed)).isEqualTo(message);
    }

    @Test
    void numericRequiredNeedsAllDigitsAndNonZero() {
        assertThat(CobolEdits.numericRequired("Zip", "1234", 5)).isEqualTo("Zip must be all numeric.");
        assertThat(CobolEdits.numericRequired("Zip", "00000", 5)).isEqualTo("Zip must not be zero.");
        assertThat(CobolEdits.numericRequired("Zip", "", 5)).isEqualTo("Zip must be supplied.");
        assertThat(CobolEdits.numericRequired("Zip", "60601", 5)).isNull();
    }

    @Test
    void alphaEditsAllowLettersAndSpacesOnly() {
        assertThat(CobolEdits.alphaRequired("City", "West Bernita")).isNull();
        assertThat(CobolEdits.alphaRequired("City", "St. Louis")).isEqualTo("City can have alphabets only.");
        assertThat(CobolEdits.alphaRequired("City", null)).isEqualTo("City must be supplied.");
        assertThat(CobolEdits.alphaOptional("Middle Name", null)).isNull();
        assertThat(CobolEdits.alphaOptional("Middle Name", "J2")).isEqualTo("Middle Name can have alphabets only.");
    }

    @Test
    void lookupsUseCslkpcdy() {
        assertThat(CobolEdits.usStateCode("NC")).isNull();
        assertThat(CobolEdits.usStateCode("nc")).isEqualTo("State: is not a valid state code");
        assertThat(CobolEdits.usStateZip("NC", "27601")).isNull();
        assertThat(CobolEdits.usStateZip("NC", "60601")).isEqualTo("Invalid zip code for state");
        assertThat(CobolEdits.ficoRange("299")).isEqualTo("FICO Score: should be between 300 and 850");
        assertThat(CobolEdits.ficoRange("850")).isNull();
    }
}
