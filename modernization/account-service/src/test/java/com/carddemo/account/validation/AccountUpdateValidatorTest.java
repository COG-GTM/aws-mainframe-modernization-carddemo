package com.carddemo.account.validation;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.account.api.dto.AccountUpdateRequest;
import com.carddemo.common.api.ApiError.FieldProblem;
import java.lang.reflect.RecordComponent;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import org.junit.jupiter.api.Test;

class AccountUpdateValidatorTest {

    private static final LocalDate TODAY = LocalDate.of(2026, 11, 2);

    static AccountUpdateRequest valid() {
        return new AccountUpdateRequest(0L, 0L, "Y", "2000.00", "300.00", "42.00", "10.00", "5.00",
                "2014-11-20", "2025-05-20", "2024-05-20", "DEFAULT", "123-45-6789", "1961-06-08", "720",
                "Immanuel", "Madeline", "Kessler", "618 Deshaun Route", "Apt. 802", "Altenwerthshire",
                "NC", "27601", "908-119-8310", "(212)693-8684", "000000000049", "0053581756", "Y");
    }

    @Test
    void aValidRequestPasses() {
        assertThat(AccountUpdateValidator.validate(valid(), "USA", TODAY)).isEmpty();
    }

    @Test
    void anOmittedSsnIsNotEdited() {
        AccountUpdateRequest r = valid();
        AccountUpdateRequest noSsn = new AccountUpdateRequest(r.accountVersion(), r.customerVersion(),
                r.activeStatus(), r.creditLimit(), r.cashCreditLimit(), r.currentBalance(),
                r.currentCycleCredit(), r.currentCycleDebit(), r.openDate(), r.expirationDate(), r.reissueDate(),
                r.groupId(), null, r.dateOfBirth(), r.ficoScore(), r.firstName(), r.middleName(), r.lastName(),
                r.addressLine1(), r.addressLine2(), r.city(), r.stateCode(), r.zip(), r.phoneNumber1(),
                r.phoneNumber2(), r.governmentIssuedId(), r.eftAccountId(), r.primaryCardHolder());
        assertThat(AccountUpdateValidator.validate(noSsn, "USA", TODAY)).isEmpty();
    }

    @Test
    void anEmptyRequestFailsEveryRequiredFieldInCobolOrder() {
        AccountUpdateRequest empty = new AccountUpdateRequest(0L, 0L, null, null, null, null, null, null, null,
                null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null,
                null, null, null);
        List<FieldProblem> problems = AccountUpdateValidator.validate(empty, null, TODAY);
        assertThat(problems.get(0)).isEqualTo(new FieldProblem("activeStatus", "Account Status must be supplied."));
        assertThat(problems).extracting(FieldProblem::field).containsExactly(
                "activeStatus", "openDate", "creditLimit", "expirationDate", "cashCreditLimit", "reissueDate",
                "currentBalance", "currentCycleCredit", "currentCycleDebit", "dateOfBirth", "ficoScore",
                "firstName", "lastName", "addressLine1", "stateCode", "zip", "city", "countryCode",
                "eftAccountId", "primaryCardHolder");
        assertThat(problems).extracting(FieldProblem::message).contains(
                "Open Date : Year must be supplied.", "Current Cycle Debit Limit must be supplied.",
                "FICO Score must be supplied.", "Address Line 1 must be supplied.", "Country must be supplied.");
    }

    @Test
    void stateZipComboRunsOnlyWhenStateAndZipPassed() {
        AccountUpdateRequest r = valid();
        AccountUpdateRequest wrongZip = new AccountUpdateRequest(0L, 0L, "Y", "2000.00", "300.00", "42.00",
                "10.00", "5.00", "2014-11-20", "2025-05-20", "2024-05-20", "DEFAULT", null, "1961-06-08", "720",
                "Immanuel", null, "Kessler", "618 Deshaun Route", null, "Altenwerthshire", "NC", "60601",
                r.phoneNumber1(), null, null, "0053581756", "Y");
        assertThat(AccountUpdateValidator.validate(wrongZip, "USA", TODAY))
                .containsExactly(new FieldProblem("zip", "Invalid zip code for state"));
    }

    @Test
    void everyMessageTheValidatorProducesIsListedInRules() {
        AccountUpdateRequest bad = new AccountUpdateRequest(0L, 0L, "y", "x", "x", "x", "x", "x", "2021-02-29",
                "1800-01-01", "2020-04-31", null, "666-00-12", "2999-01-01", "100", "J0hn", "M1", "D0e", " ",
                null, "C1ty", "ZZ", "1234", "211-000-1", "908-119-0000", null, "0000000000", "x");
        List<FieldProblem> problems = AccountUpdateValidator.validate(bad, "U5A", TODAY);
        assertThat(problems).hasSizeGreaterThan(20);
        for (FieldProblem problem : problems) {
            Set<String> listed = AccountUpdateValidator.RULES.stream()
                    .filter(rule -> rule.field().equals(problem.field()))
                    .flatMap(rule -> rule.messages().stream())
                    .collect(Collectors.toSet());
            assertThat(listed).as(problem.field()).contains(problem.message());
        }
    }

    @Test
    void rulesAreInCobolOrderAndNameRequestFields() {
        Set<String> requestFields = Arrays.stream(AccountUpdateRequest.class.getRecordComponents())
                .map(RecordComponent::getName).collect(Collectors.toSet());
        assertThat(AccountUpdateValidator.RULES).extracting(EditRule::order).isSorted();
        assertThat(AccountUpdateValidator.RULES).hasSize(25);
        AccountUpdateValidator.RULES.stream()
                .filter(rule -> !rule.field().equals("countryCode"))
                .forEach(rule -> assertThat(requestFields).contains(rule.field()));
        AccountUpdateValidator.RULES.forEach(rule -> assertThat(rule.messages()).isNotEmpty());
    }
}
