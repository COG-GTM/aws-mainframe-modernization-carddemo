package com.carddemo.web;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.then;
import static org.mockito.Mockito.never;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.card.Card;
import com.carddemo.card.CardRecord;
import com.carddemo.card.CardStatus;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.boot.test.system.CapturedOutput;
import org.springframework.boot.test.system.OutputCaptureExtension;
import org.springframework.dao.DataAccessResourceFailureException;
import org.springframework.http.HttpHeaders;
import org.springframework.test.util.ReflectionTestUtils;

/**
 * {@code docs/modernization/rules/COCRDUPC.md} against {@code PUT /api/v1/cards/{cardNumber}}: one test per rule,
 * R-id in the name. {@code confirm=false} is ENTER, {@code confirm=true} is PF5.
 */
@ExtendWith(OutputCaptureExtension.class)
class CardUpdateRulesTest extends CardWebTest {

    @Test
    void R1_noSessionIsRejected() throws Exception {
        mvc.perform(org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put(CARDS + "/" + pan(1)))
                .andExpect(status().isUnauthorized());
    }

    @Test
    void R2_onlyPutUpdates() throws Exception {
        mvc.perform(post(CARDS + "/" + pan(1)).header(HttpHeaders.AUTHORIZATION, admin()))
                .andExpect(status().isMethodNotAllowed());
    }

    @Test
    void R3_pf3ReturnsToTheCallerOrTheMainMenu() throws Exception {
        update(admin(), pan(1), form(1)).andExpect(jsonPath("$.card.exit.fromTranId").value("CCUP"))
                .andExpect(jsonPath("$.card.exit.toProgram").value("COMEN01C"));
        update(admin(), pan(1), form(1), "fromProgram", "COCRDLIC")
                .andExpect(jsonPath("$.card.exit.toProgram").value("COCRDLIC"));
    }

    @Test
    void R4_cardSelectedOnTheListIsUpdatedByItsReference() throws Exception {
        String ref = refsOf(page(admin())).get(0);
        ObjectNode form = form(1).put("embossedName", "ANIYA B VON").put("confirm", true);
        update(admin(), ref, form, "fromProgram", "COCRDLIC").andExpect(status().isOk())
                .andExpect(jsonPath("$.state").value("COMMITTED"));
    }

    @Test
    void R5_theDetailGetIsTheSearchPhase() throws Exception {
        JsonNode form = form(1);
        assertThat(form.get("version").asLong()).isZero();
        assertThat(form.get("confirm").asBoolean()).isFalse();
    }

    @Test
    void R6_afterACommitTheOldScreenIsStale() throws Exception {
        ObjectNode form = form(1).put("embossedName", "ANIYA B VON").put("confirm", true);
        update(admin(), pan(1), form).andExpect(jsonPath("$.card.version").value(1));
        update(admin(), pan(1), form.put("embossedName", "ANIYA C VON")).andExpect(status().isConflict());
    }

    @Test
    void R7_inputsAreProcessedThenTheActionDecided() throws Exception {
        change("activeStatus", "N", false).andExpect(status().isOk()).andExpect(jsonPath("$.state").value("VALIDATED"));
    }

    @Test
    void R8_responseIsTheCcupScreen() throws Exception {
        update(admin(), pan(1), form(1)).andExpect(jsonPath("$.header.tranId").value("CCUP"))
                .andExpect(jsonPath("$.header.programName").value("COCRDUPC"));
    }

    @Test
    void R9_asteriskIsLowValues() throws Exception {
        change("embossedName", "*", false).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Card name not provided"));
    }

    @Test
    void R10_bothKeysBlankIsNoInput() throws Exception {
        update(admin(), "0000000000000000", form(1).put("accountId", "*")).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("No input received"));
    }

    @Test
    void R11_accountKeyEdits() throws Exception {
        update(admin(), pan(1), form(1).put("accountId", "")).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Account number not provided"));
        update(admin(), pan(1), form(1).put("accountId", "A1")).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER"));
    }

    @Test
    void R12_cardKeyEdits() throws Exception {
        update(admin(), "0", form(1)).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Card number not provided"));
        update(admin(), "12ab", form(1)).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER"));
    }

    @Test
    void R13_noChangeAfterUpperCaseAndTrim() throws Exception {
        ObjectNode form = form(1).put("embossedName", "  aniya von ").put("expiryMonth", "3").put("confirm", true);
        update(admin(), pan(1), form).andExpect(status().isOk())
                .andExpect(jsonPath("$.state").value("SHOW"))
                .andExpect(jsonPath("$.updated").value(false))
                .andExpect(jsonPath("$.message").value("No change detected with respect to values fetched."));
        then(cards).should(never()).saveAndFlush(any());
    }

    @Test
    void R14_allEditsRunInSourceOrderAndTheFirstMessageWins() throws Exception {
        ObjectNode form = form(1).put("embossedName", "R2D2").put("activeStatus", "X").put("expiryMonth", "13")
                .put("expiryYear", "1900");
        update(admin(), pan(1), form).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.field").value("embossedName"))
                .andExpect(jsonPath("$.message").value("Card name can only contain alphabets and spaces"))
                .andExpect(jsonPath("$.invalidFields[0]").value("embossedName"))
                .andExpect(jsonPath("$.invalidFields[1]").value("activeStatus"))
                .andExpect(jsonPath("$.invalidFields[2]").value("expiryMonth"))
                .andExpect(jsonPath("$.invalidFields[3]").value("expiryYear"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"O'Brien", "Anna-Lee", "John 3", "Zoë"})
    void R15_nameAlphabeticAndSpacesOnly(String name) throws Exception {
        change("embossedName", name, false).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.field").value("embossedName"))
                .andExpect(jsonPath("$.message").value("Card name can only contain alphabets and spaces"));
    }

    @Test
    void R15_nameRequiredAndLettersWithSpacesAccepted() throws Exception {
        change("embossedName", "", false).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Card name not provided"));
        change("embossedName", "Mary Ann de la Cruz", false).andExpect(jsonPath("$.state").value("VALIDATED"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"n", "X", "", "1"})
    void R16_statusUpperCaseYOrN(String status) throws Exception {
        change("activeStatus", status, false).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.field").value("activeStatus"))
                .andExpect(jsonPath("$.message").value("Card Active Status must be Y or N"));
    }

    @Test
    void R16_lowerCaseIsAChangeThatFailsTheEditUnlessItEqualsTheStoredValue() throws Exception {
        update(admin(), pan(5), form(5).put("activeStatus", "y")).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Card Active Status must be Y or N"));
        change("activeStatus", "y", false).andExpect(jsonPath("$.state").value("SHOW"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"0", "00", "13", "ab", "", "-1"})
    void R17_monthOneToTwelve(String month) throws Exception {
        change("expiryMonth", month, false).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.field").value("expiryMonth"))
                .andExpect(jsonPath("$.message").value("Card expiry month must be between 1 and 12"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"1949", "2100", "0000", "20x5", ""})
    void R18_yearNineteenFiftyToTwentyNinetyNine(String year) throws Exception {
        change("expiryYear", year, false).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.field").value("expiryYear"))
                .andExpect(jsonPath("$.message").value("Invalid card expiry year"));
    }

    @Test
    void R18_boundaryYearsAndMonthsAreAccepted() throws Exception {
        change("expiryYear", "1950", false).andExpect(jsonPath("$.state").value("VALIDATED"));
        change("expiryYear", "2099", false).andExpect(jsonPath("$.state").value("VALIDATED"));
        change("expiryMonth", "1", false).andExpect(jsonPath("$.state").value("VALIDATED"));
        change("expiryMonth", "12", false).andExpect(jsonPath("$.state").value("VALIDATED"));
    }

    @Test
    void R19_unchangedCardShowsTheDetailsState() throws Exception {
        update(admin(), pan(1), form(1)).andExpect(jsonPath("$.state").value("SHOW"))
                .andExpect(jsonPath("$.infoMessage").value("Details of selected card shown above"));
    }

    @Test
    void R20_enterWithValidChangesValidatesWithoutWriting() throws Exception {
        change("expiryYear", "2030", false).andExpect(status().isOk())
                .andExpect(jsonPath("$.state").value("VALIDATED"))
                .andExpect(jsonPath("$.updated").value(false))
                .andExpect(jsonPath("$.infoMessage").value("Changes validated.Press F5 to save"))
                .andExpect(jsonPath("$.card.expiryYear").value("2025"));
        then(cards).should(never()).saveAndFlush(any());
    }

    @Test
    void R21_editErrorsAreReportedAndNothingIsWritten() throws Exception {
        change("expiryMonth", "13", true).andExpect(status().isBadRequest());
        then(cards).should(never()).saveAndFlush(any());
    }

    @Test
    void R22_pf5CommitsOrReportsTheConcurrentChange() throws Exception {
        change("activeStatus", "N", true).andExpect(status().isOk())
                .andExpect(jsonPath("$.state").value("COMMITTED"))
                .andExpect(jsonPath("$.updated").value(true))
                .andExpect(jsonPath("$.card.activeStatus").value("N"));
        update(admin(), pan(2), form(2).put("version", 7).put("activeStatus", "N").put("confirm", true))
                .andExpect(status().isConflict())
                .andExpect(jsonPath("$.code").value("CHANGED"))
                .andExpect(jsonPath("$.message").value("Record changed by some one else. Please review"));
    }

    @Test
    void R23_enterAgainStaysValidatedWithoutWriting() throws Exception {
        change("expiryYear", "2031", false).andExpect(jsonPath("$.state").value("VALIDATED"));
        change("expiryYear", "2031", false).andExpect(jsonPath("$.state").value("VALIDATED"));
        then(cards).should(never()).saveAndFlush(any());
    }

    @Test
    void R24_unexpectedFailureAbends() throws Exception {
        given(cards.saveAndFlush(any(Card.class))).willThrow(new IllegalStateException("boom"));
        assertThatThrownBy(() -> change("activeStatus", "N", true)).hasRootCauseInstanceOf(IllegalStateException.class);
    }

    @Test
    void R25_readUpperCasesTheNameAndSplitsTheExpiryAndDoesNotCrossCheckTheAccountForAnAdmin() throws Exception {
        JsonNode form = form(1);
        assertThat(form.get("embossedName").asText()).isEqualTo("ANIYA VON");
        assertThat(form.get("expiryMonth").asText()).isEqualTo("03");
        assertThat(form.get("expiryYear").asText()).isEqualTo("2025");
        update(admin(), pan(12), form(12).put("accountId", "1").put("activeStatus", "N").put("confirm", true))
                .andExpect(status().isOk()).andExpect(jsonPath("$.card.accountId").value("00000000002"));
    }

    @Test
    void R26_missingCardIsNotFound() throws Exception {
        update(admin(), "9999999999999999", form(1)).andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("Did not find cards for this search condition"));
    }

    @Test
    void R27_otherReadErrorAbends() throws Exception {
        ObjectNode form = form(1);
        given(cards.findById(anyString())).willThrow(new DataAccessResourceFailureException("down"));
        update(admin(), pan(1), form).andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.message").value(
                        "USER ABEND U0999: File Error: READ     on CARDDAT   returned RESP 000000017 ,RESP2 000000000"));
    }

    @Test
    void R28_lockFailureIsCouldNotLock() throws Exception {
        given(cards.lockVersion(anyString())).willReturn(Optional.empty());
        change("activeStatus", "N", true).andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.message").value("USER ABEND U0999: Could not lock record for update"));
        given(cards.lockVersion(anyString())).willThrow(new DataAccessResourceFailureException("locked"));
        change("activeStatus", "N", true)
                .andExpect(jsonPath("$.message").value("USER ABEND U0999: Could not lock record for update"));
    }

    @Test
    void R29_recordChangedBetweenReadAndLockIsNotRewritten() throws Exception {
        given(cards.lockVersion(anyString())).willReturn(Optional.of(5L));
        change("activeStatus", "N", true).andExpect(status().isConflict())
                .andExpect(jsonPath("$.code").value("CHANGED"));
        then(cards).should(never()).saveAndFlush(any());
    }

    @Test
    void R30_rewriteKeepsTypedNameDayCvvAndAccount() throws Exception {
        ObjectNode form = form(1).put("embossedName", "Aniya B Von ").put("expiryMonth", "7")
                .put("expiryYear", "2031").put("activeStatus", "N").put("confirm", true);
        update(admin(), pan(1), form).andExpect(status().isOk());
        Card stored = store.get(pan(1));
        assertThat(stored.getEmbossedName()).isEqualTo("Aniya B Von");
        assertThat(stored.getExpirationDate()).isEqualTo("2031-07-14");
        assertThat(stored.getActiveStatus().code()).isEqualTo("N");
        assertThat(stored.getCvvCd()).isEqualTo(101);
        assertThat(stored.getAcctId()).isEqualTo(1L);
    }

    @Test
    void R31_rewriteFailureIsUpdateFailed() throws Exception {
        given(cards.saveAndFlush(any(Card.class))).willThrow(new DataAccessResourceFailureException("down"));
        change("activeStatus", "N", true).andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.message").value("USER ABEND U0999: Update of record failed"));
    }

    @Test
    void R32_infoTextFollowsTheState() throws Exception {
        change("activeStatus", "N", false).andExpect(jsonPath("$.infoMessage")
                .value("Changes validated.Press F5 to save"));
        change("activeStatus", "N", true).andExpect(jsonPath("$.infoMessage").value("Changes committed to database"))
                .andExpect(jsonPath("$.card.infoMessage").value("Changes committed to database"));
    }

    @Test
    void R33_keysAreNotChangedAndOnlyPf5Writes() throws Exception {
        update(admin(), pan(1), form(1).put("accountId", "2").put("activeStatus", "N").put("confirm", true))
                .andExpect(status().isOk());
        assertThat(store.get(pan(1)).getAcctId()).isEqualTo(1L);
        assertThat(store.get(pan(1)).getCardNum()).isEqualTo(pan(1));
    }

    @Test
    void expiryDayIsClampedToTheNewMonth() throws Exception {
        expiringOn(1, "2025-01-31");
        expiringOn(2, "2025-03-31");
        expiringOn(3, "2025-05-31");
        update(admin(), pan(1), form(1).put("expiryMonth", "2").put("expiryYear", "2027").put("confirm", true))
                .andExpect(status().isOk());
        update(admin(), pan(2), form(2).put("expiryMonth", "02").put("expiryYear", "2028").put("confirm", true))
                .andExpect(status().isOk());
        update(admin(), pan(3), form(3).put("expiryMonth", "4").put("expiryYear", "2030").put("confirm", true))
                .andExpect(status().isOk());
        assertThat(store.get(pan(1)).getExpirationDate()).isEqualTo("2027-02-28");
        assertThat(store.get(pan(2)).getExpirationDate()).isEqualTo("2028-02-29");
        assertThat(store.get(pan(3)).getExpirationDate()).isEqualTo("2030-04-30");
    }

    private void expiringOn(int i, String date) {
        Card card = Card.from(new CardRecord(pan(i), account(i), 100 + i, "Holder Name", date,
                CardStatus.fromCode("Y")));
        ReflectionTestUtils.setField(card, "version", 0L);
        store.put(card.getCardNum(), card);
    }

    @Test
    void versionIsRequired() throws Exception {
        ObjectNode form = form(1);
        form.remove("version");
        update(admin(), pan(1), form).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.field").value("version"));
    }

    @Test
    void userOnlyUpdatesCardsOfTheAccountGiven() throws Exception {
        update(user(), pan(12), form(12).put("accountId", "1").put("activeStatus", "N").put("confirm", true))
                .andExpect(status().isNotFound());
        update(user(), pan(12), form(12).put("activeStatus", "N").put("confirm", true))
                .andExpect(status().isOk()).andExpect(jsonPath("$.state").value("COMMITTED"));
    }

    @Test
    void logsMaskTheCardNumber(CapturedOutput output) throws Exception {
        change("activeStatus", "N", true).andExpect(status().isOk());
        assertThat(logLines(output)).contains("COCRDUPC REWRITE card " + masked(1)).doesNotContain(pan(1));
        ReflectionTestUtils.setField(store.get(pan(2)), "version", 3L);
        update(admin(), pan(2), form(2).put("version", 0).put("activeStatus", "N").put("confirm", true))
                .andExpect(status().isConflict());
        assertThat(logLines(output)).doesNotContain(pan(2));
    }

    /** Log records only: MockMvc also prints the request/response, and the detail response carries the full PAN. */
    private static String logLines(CapturedOutput output) {
        return output.getAll().lines().filter(l -> l.contains(" --- [carddemo-app] "))
                .collect(java.util.stream.Collectors.joining("\n"));
    }
}
