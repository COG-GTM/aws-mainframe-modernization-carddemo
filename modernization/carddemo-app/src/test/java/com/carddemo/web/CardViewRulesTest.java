package com.carddemo.web;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.fasterxml.jackson.databind.JsonNode;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.dao.DataAccessResourceFailureException;
import org.springframework.http.HttpHeaders;

/**
 * {@code docs/modernization/rules/COCRDSLC.md} against {@code GET /api/v1/cards/{cardNumber}} and the account path
 * {@code GET /api/v1/cards/by-account/{accountId}}: one test per rule, R-id in the name.
 */
class CardViewRulesTest extends CardWebTest {

    @Test
    void R1_noSessionIsRejectedAndNoMessageAfterASuccessfulRead() throws Exception {
        mvc.perform(get(CARDS + "/" + pan(1)).param("accountId", "1")).andExpect(status().isUnauthorized());
        view(admin(), pan(1), "1").andExpect(status().isOk()).andExpect(jsonPath("$.message").value(""));
    }

    @Test
    void R2_everySearchStartsFresh() throws Exception {
        view(admin(), pan(1), "1").andExpect(status().isOk());
        view(admin(), pan(2), "1").andExpect(jsonPath("$.cardNumber").value(pan(2)));
    }

    @Test
    void R3_onlyEnterAndPf3AreHandled() throws Exception {
        mvc.perform(post(CARDS + "/" + pan(1)).header(HttpHeaders.AUTHORIZATION, admin()))
                .andExpect(status().isMethodNotAllowed())
                .andExpect(jsonPath("$.code").value("INVALID_KEY"));
    }

    @Test
    void R4_pf3ReturnsToTheCallerOrTheMainMenu() throws Exception {
        view(admin(), pan(1), "1").andExpect(jsonPath("$.exit.fromTranId").value("CCDL"))
                .andExpect(jsonPath("$.exit.fromProgram").value("COCRDSLC"))
                .andExpect(jsonPath("$.exit.toTranId").value("CM00"))
                .andExpect(jsonPath("$.exit.toProgram").value("COMEN01C"));
        view(admin(), pan(1), "1", "fromProgram", "COCRDLIC").andExpect(jsonPath("$.exit.toTranId").value("CCLI"))
                .andExpect(jsonPath("$.exit.toProgram").value("COCRDLIC"));
    }

    @Test
    void R5_cardSelectedOnTheListIsReadByItsReference() throws Exception {
        String ref = refsOf(page(admin())).get(3);
        view(admin(), ref, "00000000001", "fromProgram", "COCRDLIC").andExpect(status().isOk())
                .andExpect(jsonPath("$.cardNumber").value(pan(4)))
                .andExpect(jsonPath("$.cardRef").value(ref));
    }

    @Test
    void R6_withoutKeysTheSearchPromptsForTheAccountFirst() throws Exception {
        view(admin(), pan(1), null).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.field").value("accountId"))
                .andExpect(jsonPath("$.message").value("Account number not provided"));
    }

    @Test
    void R7_reentryEditsThenReads() throws Exception {
        view(admin(), "x", "1").andExpect(status().isBadRequest());
        view(admin(), pan(1), "1").andExpect(status().isOk());
    }

    @Test
    void R8_otherScenariosAreRejected() throws Exception {
        mvc.perform(delete(CARDS + "/" + pan(1)).header(HttpHeaders.AUTHORIZATION, admin()))
                .andExpect(status().isMethodNotAllowed());
    }

    @Test
    void R9_responseIsTheCcdlScreen() throws Exception {
        view(admin(), pan(1), "1").andExpect(jsonPath("$.header.tranId").value("CCDL"))
                .andExpect(jsonPath("$.header.programName").value("COCRDSLC"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"*", " "})
    void R10_asteriskOrSpacesAreNotSupplied(String blank) throws Exception {
        view(admin(), pan(1), blank).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Account number not provided"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"", "0", "00000000000"})
    void R11_blankOrZeroAccountIsNotProvided(String blank) throws Exception {
        view(admin(), pan(1), blank).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.field").value("accountId"))
                .andExpect(jsonPath("$.message").value("Account number not provided"));
    }

    @Test
    void R12_accountMustBeNumeric() throws Exception {
        view(admin(), pan(1), "1A").andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.field").value("accountId"))
                .andExpect(jsonPath("$.message").value("ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER"));
    }

    @Test
    void R13_blankOrZeroCardIsNotProvided() throws Exception {
        view(admin(), "0000000000000000", "1").andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.field").value("cardNumber"))
                .andExpect(jsonPath("$.message").value("Card number not provided"));
    }

    @Test
    void R14_cardMustBeNumeric() throws Exception {
        view(admin(), "4000x", "1").andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.field").value("cardNumber"))
                .andExpect(jsonPath("$.message").value("CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER"));
    }

    @Test
    void R15_bothBlankIsNoInput() throws Exception {
        view(admin(), "*", "*").andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("No input received"))
                .andExpect(jsonPath("$.invalidFields.length()").value(2));
    }

    @Test
    void R16_foundCardIsDisplayedAndTheAccountIsNotCrossCheckedForAnAdmin() throws Exception {
        view(admin(), pan(12), "1").andExpect(status().isOk())
                .andExpect(jsonPath("$.infoMessage").value("   Displaying requested details"))
                .andExpect(jsonPath("$.accountId").value("00000000002"));
    }

    @Test
    void R17_missingCardIsNotFound() throws Exception {
        view(admin(), "9999999999999999", "1").andExpect(status().isNotFound())
                .andExpect(jsonPath("$.code").value("NOTFND"))
                .andExpect(jsonPath("$.message").value("Did not find cards for this search condition"));
    }

    @Test
    void R18_otherFileErrorAbends() throws Exception {
        given(cards.findById(anyString())).willThrow(new DataAccessResourceFailureException("down"));
        view(admin(), pan(1), "1").andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.message").value(
                        "USER ABEND U0999: File Error: READ     on CARDDAT   returned RESP 000000017 ,RESP2 000000000"));
    }

    @Test
    void R19_cardFieldsAreDisplayedWithTheFullCardNumber() throws Exception {
        view(admin(), pan(1), "1").andExpect(jsonPath("$.accountId").value("00000000001"))
                .andExpect(jsonPath("$.cardNumber").value(pan(1)))
                .andExpect(jsonPath("$.embossedName").value("Aniya Von"))
                .andExpect(jsonPath("$.expiryMonth").value("03"))
                .andExpect(jsonPath("$.expiryYear").value("2025"))
                .andExpect(jsonPath("$.activeStatus").value("Y"));
    }

    @Test
    void R20_fromTheListTheKeysTravelAsReferenceAndPf3GoesBack() throws Exception {
        JsonNode detail = body(view(admin(), refsOf(page(admin())).get(0), "1", "fromProgram", "COCRDLIC"));
        assertThat(detail.get("cardRef").asText()).isNotBlank();
        assertThat(detail.get("exit").get("toProgram").asText()).isEqualTo("COCRDLIC");
    }

    @Test
    void R21_screenCarriesVersionAndTheUpdateForm() throws Exception {
        view(admin(), pan(1), "1").andExpect(jsonPath("$.version").value(0))
                .andExpect(jsonPath("$.updateForm.version").value(0))
                .andExpect(jsonPath("$.updateForm.accountId").value("00000000001"))
                .andExpect(jsonPath("$.updateForm.embossedName").value("ANIYA VON"));
    }

    @Test
    void R22_unexpectedFailureAbendsWithTheCardDemoCode() throws Exception {
        given(cards.findById(anyString())).willThrow(new DataAccessResourceFailureException("down"));
        view(admin(), pan(1), "1").andExpect(jsonPath("$.code").value("ABEND"))
                .andExpect(jsonPath("$.abendCode").value("U0999"));
    }

    @Test
    void userOnlySeesCardsOfTheAccountGiven() throws Exception {
        view(user(), pan(3), "1").andExpect(status().isOk()).andExpect(jsonPath("$.cardNumber").value(pan(3)));
        view(user(), pan(12), "1").andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("Did not find cards for this search condition"));
    }

    @Test
    void accountPathReadsTheFirstCardOfTheAccount() throws Exception {
        mvc.perform(get(CARDS + "/by-account/2").header(HttpHeaders.AUTHORIZATION, user()))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.cardNumber").value(pan(10)))
                .andExpect(jsonPath("$.accountId").value("00000000002"));
        mvc.perform(get(CARDS + "/by-account/99").header(HttpHeaders.AUTHORIZATION, user()))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("Did not find this account in cards database"));
        mvc.perform(get(CARDS + "/by-account/abc").header(HttpHeaders.AUTHORIZATION, user()))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER"));
        mvc.perform(get(CARDS + "/by-account/0").header(HttpHeaders.AUTHORIZATION, user()))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Account number not provided"));
    }
}
