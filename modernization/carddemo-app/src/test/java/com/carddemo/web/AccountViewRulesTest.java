package com.carddemo.web;

import static org.hamcrest.Matchers.contains;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.user.UserType;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.dao.DataAccessResourceFailureException;
import org.springframework.http.HttpHeaders;

/**
 * {@code docs/modernization/rules/COACTVWC.md} against {@code GET /api/v1/accounts/{id}}: one test per rule, R-id in
 * the name. Screen-only rules (map attributes, SEND/RETURN) are asserted through their REST equivalents.
 */
class AccountViewRulesTest extends AccountWebTest {

    @Test
    void R1_noSessionContextIsRejectedAndEveryRequestStartsFresh() throws Exception {
        mvc.perform(get(ACCOUNTS + "/1"))
                .andExpect(status().isUnauthorized())
                .andExpect(jsonPath("$.code").value("SIGNON_REQUIRED"));
        view("1").andExpect(status().isOk()).andExpect(jsonPath("$.message").value(""));
    }

    @Test
    void R2_pf3ReturnsToTheMainMenu() throws Exception {
        view("1").andExpect(jsonPath("$.exit.fromTranId").value("CAVW"))
                .andExpect(jsonPath("$.exit.fromProgram").value("COACTVWC"))
                .andExpect(jsonPath("$.exit.toTranId").value("CM00"))
                .andExpect(jsonPath("$.exit.toProgram").value("COMEN01C"))
                .andExpect(jsonPath("$.exit.pgmContext").value("ENTER"));
    }

    @Test
    void R3_screenCarriesThePrompt() throws Exception {
        view("1").andExpect(jsonPath("$.infoMessage").value("Enter or update id of account to display"));
    }

    @Test
    void R4_validInputIsReadAndDisplayed() throws Exception {
        view("00000000001").andExpect(status().isOk()).andExpect(jsonPath("$.acctId").value("00000000001"));
    }

    @Test
    void R5_otherScenariosAreRejected() throws Exception {
        mvc.perform(post(ACCOUNTS + "/1").header(HttpHeaders.AUTHORIZATION, user()))
                .andExpect(status().isMethodNotAllowed());
    }

    @Test
    void R6_responseIsTheCavwScreen() throws Exception {
        view("1").andExpect(jsonPath("$.header.tranId").value("CAVW"))
                .andExpect(jsonPath("$.header.programName").value("COACTVWC"))
                .andExpect(jsonPath("$.header.currentDate").value("07/06/22"));
    }

    @Test
    void R7_blankAccountIdIsNoInput() throws Exception {
        view("*").andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.code").value("INVREQ"))
                .andExpect(jsonPath("$.field").value("acctId"))
                .andExpect(jsonPath("$.message").value("No input received"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"abc", "0", "00000000000", "123456789012", "1-2"})
    void R8_notNumericOrZeroIsRejected(String id) throws Exception {
        view(id).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.field").value("acctId"))
                .andExpect(jsonPath("$.message").value("Account Filter must  be a non-zero 11 digit number"));
    }

    @Test
    void R9_numericNonZeroIdIsAccepted() throws Exception {
        view("1").andExpect(status().isOk()).andExpect(jsonPath("$.acctId").value("00000000001"));
    }

    @Test
    void R10_crossReferenceSuppliesCustomerAndCard() throws Exception {
        view("1").andExpect(jsonPath("$.custId").value("000000001"))
                .andExpect(jsonPath("$.cardNum").value(CARD_1))
                .andExpect(jsonPath("$.cardNumbers", contains(CARD_1, CARD_2)));
    }

    @Test
    void R11_accountNotInCrossReference() throws Exception {
        given(xrefs.findFirstByAcctIdOrderByCardNumAsc(99L)).willReturn(Optional.empty());
        view("99").andExpect(status().isNotFound())
                .andExpect(jsonPath("$.code").value("NOTFND"))
                .andExpect(jsonPath("$.message").value(
                        "Account:00000000099 not found in Cross ref file.  Resp:000000013  Reas:0000"));
    }

    @Test
    void R12_otherCrossReferenceErrorIsAFileError() throws Exception {
        given(xrefs.findFirstByAcctIdOrderByCardNumAsc(1L))
                .willThrow(new DataAccessResourceFailureException("down"));
        view("1").andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.code").value("ABEND"))
                .andExpect(jsonPath("$.message").value(
                        "USER ABEND U0999: File Error: READ     on CXACAIX   returned RESP 000000017 ,RESP2 000000000"));
    }

    @Test
    void R13_accountNotInMaster() throws Exception {
        given(accounts.findById(1L)).willReturn(Optional.empty());
        view("1").andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value(
                        "Account:00000000001 not found in Acct Master file.Resp:000000013  Reas:0000"));
    }

    @Test
    void R14_customerNotInMaster() throws Exception {
        given(customers.findById(1)).willReturn(Optional.empty());
        view("1").andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value(
                        "CustId:000000001 not found in customer master.Resp: 000000013  REAS:0000000"));
    }

    @Test
    void R15_accountFieldsAreDisplayed() throws Exception {
        view("1").andExpect(jsonPath("$.activeStatus").value("Y"))
                .andExpect(jsonPath("$.currentBalance").value(1940.00))
                .andExpect(jsonPath("$.creditLimit").value(20200.00))
                .andExpect(jsonPath("$.cashCreditLimit").value(10200.00))
                .andExpect(jsonPath("$.currentCycleCredit").value(0.00))
                .andExpect(jsonPath("$.currentCycleDebit").value(0.00))
                .andExpect(jsonPath("$.openDate").value("2014-11-20"))
                .andExpect(jsonPath("$.expirationDate").value("2025-05-20"))
                .andExpect(jsonPath("$.reissueDate").value("2025-05-20"))
                .andExpect(jsonPath("$.groupId").value("A000000000"));
    }

    @Test
    void R16_customerFieldsAreDisplayed() throws Exception {
        view("1").andExpect(jsonPath("$.custId").value("000000001"))
                .andExpect(jsonPath("$.ssn").value("020-97-3888"))
                .andExpect(jsonPath("$.ficoScore").value("704"))
                .andExpect(jsonPath("$.dateOfBirth").value("1961-06-08"))
                .andExpect(jsonPath("$.firstName").value("Immanuel"))
                .andExpect(jsonPath("$.middleName").value("Madeline"))
                .andExpect(jsonPath("$.lastName").value("Kessler"))
                .andExpect(jsonPath("$.addressLine1").value("618 Deshaun Route"))
                .andExpect(jsonPath("$.addressLine2").value("Apt. 802"))
                .andExpect(jsonPath("$.city").value("Altenwerthshire"))
                .andExpect(jsonPath("$.state").value("NC"))
                .andExpect(jsonPath("$.zip").value("27601"))
                .andExpect(jsonPath("$.country").value("USA"))
                .andExpect(jsonPath("$.phone1").value("(908)119-8310"))
                .andExpect(jsonPath("$.phone2").value("(212)693-8684"))
                .andExpect(jsonPath("$.governmentId").value("00000000000049368437"))
                .andExpect(jsonPath("$.eftAccountId").value("0053581756"))
                .andExpect(jsonPath("$.primaryCardHolder").value("Y"));
    }

    @Test
    void R17_noErrorMessageAfterASuccessfulRead() throws Exception {
        view("1").andExpect(jsonPath("$.message").value(""))
                .andExpect(jsonPath("$.infoMessage").value("Enter or update id of account to display"));
    }

    @Test
    void R18_errorsPointAtTheAccountIdField() throws Exception {
        view("x").andExpect(jsonPath("$.field").value("acctId"));
        given(xrefs.findFirstByAcctIdOrderByCardNumAsc(5L)).willReturn(Optional.empty());
        view("5").andExpect(jsonPath("$.field").doesNotExist());
    }

    @Test
    void R19_responseCarriesWhatTheNextInteractionNeeds() throws Exception {
        view("1").andExpect(jsonPath("$.accountVersion").value(0))
                .andExpect(jsonPath("$.customerVersion").value(0))
                .andExpect(jsonPath("$.updateForm.accountVersion").value(0))
                .andExpect(jsonPath("$.updateForm.firstName").value("Immanuel"));
    }

    @Test
    void R20_unexpectedFailureAbends() throws Exception {
        given(customers.findById(1)).willThrow(new DataAccessResourceFailureException("down"));
        view("1").andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.abendCode").value("U0999"))
                .andExpect(jsonPath("$.message").value(
                        "USER ABEND U0999: File Error: READ     on CUSTDAT   returned RESP 000000017 ,RESP2 000000000"));
    }

    @Test
    void anySignedOnUserMayViewAnyAccount() throws Exception {
        mvc.perform(get(ACCOUNTS + "/1").header(HttpHeaders.AUTHORIZATION, bearer("ADMIN001", UserType.ADMIN)))
                .andExpect(status().isOk());
        view("1").andExpect(status().isOk());
    }
}
