package com.carddemo.web;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.Collections;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.dao.DataAccessResourceFailureException;
import org.springframework.data.domain.Limit;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;

/**
 * {@code docs/modernization/rules/COCRDLIC.md} against {@code GET /api/v1/cards} and {@code POST
 * /api/v1/cards/selection}: one test per rule, R-id in the name; screen-only rules are asserted through their REST
 * equivalents.
 */
class CardListRulesTest extends CardWebTest {

    @Test
    void R1_freshStartIsTheFirstPageAndNeedsASignOn() throws Exception {
        mvc.perform(get(CARDS)).andExpect(status().isUnauthorized())
                .andExpect(jsonPath("$.code").value("SIGNON_REQUIRED"));
        JsonNode page = page(admin());
        assertThat(maskedOf(page)).isEqualTo(maskedRange(1, 7));
        assertThat(page.get("hasPreviousPage").asBoolean()).isFalse();
    }

    @Test
    void R2_navigationContextIsNotAFilterOnlyTypedFiltersAre() throws Exception {
        JsonNode page = page(admin());
        assertThat(page.get("accountId").isNull()).isTrue();
        assertThat(page.get("cardNumber").isNull()).isTrue();
        assertThat(accountsOf(page)).contains("00000000001");
        assertThat(accountsOf(page(admin(), "accountId", "2"))).containsOnly("00000000002");
    }

    @Test
    void R3_inputsAreEditedBeforeTheKeyIsDispatched() throws Exception {
        String ref = refsOf(page(admin())).get(6);
        list(admin(), "accountId", "12A", "after", ref).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.field").value("accountId"));
    }

    @Test
    void R4_pf3ReturnsToTheMainMenu() throws Exception {
        list(admin()).andExpect(jsonPath("$.exit.fromTranId").value("CCLI"))
                .andExpect(jsonPath("$.exit.fromProgram").value("COCRDLIC"))
                .andExpect(jsonPath("$.exit.toTranId").value("CM00"))
                .andExpect(jsonPath("$.exit.toProgram").value("COMEN01C"));
    }

    @Test
    void R5_anyRequestWithoutACursorStartsAtTheFirstPageAgain() throws Exception {
        JsonNode third = page(admin(), "after", page(admin(), "after", page(admin()).get("nextPage").asText())
                .get("nextPage").asText());
        assertThat(third.get("hasNextPage").asBoolean()).isFalse();
        assertThat(maskedOf(page(admin()))).isEqualTo(maskedRange(1, 7));
    }

    @Test
    void R6_inputErrorIsReportedWithoutRows() throws Exception {
        list(admin(), "cardNumber", "x").andExpect(status().isBadRequest())
                .andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_PROBLEM_JSON))
                .andExpect(jsonPath("$.rows").doesNotExist());
    }

    @Test
    void R7_pf7OnTheFirstPageRereadsItWithNoPreviousPages() throws Exception {
        String first = refsOf(page(admin())).get(0);
        list(admin(), "before", first).andExpect(status().isOk())
                .andExpect(jsonPath("$.rows.length()").value(7))
                .andExpect(jsonPath("$.rows[0].cardNumber").value(masked(1)))
                .andExpect(jsonPath("$.hasPreviousPage").value(false))
                .andExpect(jsonPath("$.message").value("NO PREVIOUS PAGES TO DISPLAY"));
    }

    @Test
    void R8_everyRequestIsAFreshStartOfItsOwn() throws Exception {
        assertThat(page(user(), "accountId", "1")).isEqualTo(page(user(), "accountId", "1"));
    }

    @Test
    void R9_pf8ShowsTheRecordsAfterTheLastRowAndStopsAtTheEnd() throws Exception {
        JsonNode first = page(admin());
        JsonNode second = page(admin(), "after", first.get("nextPage").asText());
        assertThat(maskedOf(second)).isEqualTo(maskedRange(8, 14));
        JsonNode third = page(admin(), "after", second.get("nextPage").asText());
        assertThat(maskedOf(third)).isEqualTo(maskedRange(15, 17));
        assertThat(third.get("nextPage").isNull()).isTrue();
        list(admin(), "after", refsOf(third).get(2)).andExpect(jsonPath("$.rows.length()").value(0))
                .andExpect(jsonPath("$.message").value("NO MORE PAGES TO DISPLAY"));
    }

    @Test
    void R10_pf7ShowsTheRecordsBeforeTheFirstRow() throws Exception {
        JsonNode second = page(admin(), "after", page(admin()).get("nextPage").asText());
        JsonNode back = page(admin(), "before", second.get("previousPage").asText());
        assertThat(maskedOf(back)).isEqualTo(maskedRange(1, 7));
        assertThat(back.get("hasPreviousPage").asBoolean()).isFalse();
        assertThat(back.get("hasNextPage").asBoolean()).isTrue();
    }

    @Test
    void R11_selectionSRoutesToTheCardDetail() throws Exception {
        List<String> refs = refsOf(page(admin()));
        select(admin(), refs, List.of(" ", "S", "", "", "", "", "")).andExpect(status().isOk())
                .andExpect(jsonPath("$.navigation.fromTranId").value("CCLI"))
                .andExpect(jsonPath("$.navigation.fromProgram").value("COCRDLIC"))
                .andExpect(jsonPath("$.navigation.toTranId").value("CCDL"))
                .andExpect(jsonPath("$.navigation.toProgram").value("COCRDSLC"))
                .andExpect(jsonPath("$.navigation.acctId").value(1))
                .andExpect(jsonPath("$.navigation.cardNum").doesNotExist())
                .andExpect(jsonPath("$.cardRef").value(refs.get(1)))
                .andExpect(jsonPath("$.next").value(CARDS + "/" + refs.get(1)
                        + "?accountId=00000000001&fromProgram=COCRDLIC"));
    }

    @Test
    void R12_selectionURoutesToTheCardUpdate() throws Exception {
        List<String> refs = refsOf(page(admin(), "accountId", "2"));
        select(admin(), refs, List.of("", "", "", "", "", "", "U")).andExpect(status().isOk())
                .andExpect(jsonPath("$.navigation.toTranId").value("CCUP"))
                .andExpect(jsonPath("$.navigation.toProgram").value("COCRDUPC"))
                .andExpect(jsonPath("$.navigation.acctId").value(2))
                .andExpect(jsonPath("$.cardRef").value(refs.get(6)));
    }

    @Test
    void selectionByUserIsScopedToTheAccountInContext() throws Exception {
        List<String> refs = refsOf(page(admin()));
        List<String> first = List.of("S", "", "", "", "", "", "");
        select(user(), refs, first).andExpect(status().isForbidden())
                .andExpect(jsonPath("$.code").value("NOTAUTH"));
        select(user(), "2", refs, first).andExpect(status().isNotFound())
                .andExpect(jsonPath("$.code").value("NOTFND"));
        select(user(), "1", refs, first).andExpect(status().isOk())
                .andExpect(jsonPath("$.navigation.acctId").value(1));
    }

    @Test
    void R13_noSelectionStaysOnTheList() throws Exception {
        List<String> refs = refsOf(page(admin()));
        select(admin(), refs, Collections.nCopies(7, "")).andExpect(status().isOk())
                .andExpect(jsonPath("$.navigation").doesNotExist())
                .andExpect(jsonPath("$.infoMessage").value("TYPE S FOR DETAIL, U TO UPDATE ANY RECORD"));
    }

    @Test
    void R14_responseIsTheCcliScreen() throws Exception {
        list(admin()).andExpect(jsonPath("$.header.tranId").value("CCLI"))
                .andExpect(jsonPath("$.header.programName").value("COCRDLIC"))
                .andExpect(jsonPath("$.header.currentDate").value("07/06/22"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"", " ", "0", "00000000000"})
    void R15_blankOrZeroAccountIsNoFilter(String blank) throws Exception {
        assertThat(maskedOf(page(admin(), "accountId", blank))).isEqualTo(maskedRange(1, 7));
    }

    @ParameterizedTest
    @ValueSource(strings = {"abc", "1-2", "123456789012"})
    void R15_accountFilterMustBeNumeric(String account) throws Exception {
        list(admin(), "accountId", account).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.code").value("INVREQ"))
                .andExpect(jsonPath("$.field").value("accountId"))
                .andExpect(jsonPath("$.message").value("ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER"));
    }

    @Test
    void R16_cardFilterMustBeNumericAndTheAccountMessageWins() throws Exception {
        list(admin(), "cardNumber", "12345678901234567").andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.field").value("cardNumber"))
                .andExpect(jsonPath("$.message").value("CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER"));
        list(admin(), "accountId", "x", "cardNumber", "y").andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER"))
                .andExpect(jsonPath("$.invalidFields[0]").value("accountId"))
                .andExpect(jsonPath("$.invalidFields[1]").value("cardNumber"));
    }

    @Test
    void R17_onlyOneUpperCaseSelectionIsAccepted() throws Exception {
        List<String> refs = refsOf(page(admin()));
        select(admin(), refs, List.of("S", "", "U", "", "", "", "")).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("PLEASE SELECT ONLY ONE RECORD TO VIEW OR UPDATE"))
                .andExpect(jsonPath("$.invalidFields[0]").value("rows[0].action"))
                .andExpect(jsonPath("$.invalidFields[1]").value("rows[2].action"));
        select(admin(), refs, List.of("", "s", "", "", "", "", "")).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.field").value("rows[1].action"))
                .andExpect(jsonPath("$.message").value("INVALID ACTION CODE"));
    }

    @Test
    void R18_browseIsInCardNumberOrderFromTheStart() throws Exception {
        List<String> masked = maskedOf(page(admin()));
        assertThat(masked).hasSize(7).isEqualTo(maskedRange(1, 7));
    }

    @Test
    void R19_accountAndCardFiltersBothApply() throws Exception {
        assertThat(accountsOf(page(admin(), "accountId", "1"))).hasSize(7).containsOnly("00000000001");
        assertThat(maskedOf(page(admin(), "accountId", "1", "cardNumber", pan(3)))).containsExactly(masked(3));
        assertThat(maskedOf(page(admin(), "accountId", "2", "cardNumber", pan(3)))).isEmpty();
        assertThat(maskedOf(page(admin(), "cardNumber", pan(12)))).containsExactly(masked(12));
    }

    @Test
    void R20_sevenRowsAndALookAheadDecideTheNextPage() throws Exception {
        JsonNode full = page(admin(), "accountId", "1");
        assertThat(full.get("hasNextPage").asBoolean()).isTrue();
        assertThat(full.get("nextPage").asText()).isEqualTo(refsOf(full).get(6));
        assertThat(full.get("infoMessage").asText()).isEqualTo("TYPE S FOR DETAIL, U TO UPDATE ANY RECORD");
        JsonNode exactlySeven = page(admin(), "accountId", "2");
        assertThat(maskedOf(exactlySeven)).isEqualTo(maskedRange(10, 16));
        assertThat(exactlySeven.get("hasNextPage").asBoolean()).isFalse();
        assertThat(exactlySeven.get("message").asText()).isEqualTo("NO MORE RECORDS TO SHOW");
    }

    @Test
    void R21_endOfFileBeforeSevenRowsAndTheEmptySearch() throws Exception {
        JsonNode last = page(admin(), "accountId", "1", "after", page(admin(), "accountId", "1").get("nextPage")
                .asText());
        assertThat(maskedOf(last)).isEqualTo(maskedRange(8, 9));
        assertThat(last.get("message").asText()).isEqualTo("NO MORE RECORDS TO SHOW");
        JsonNode empty = page(admin(), "accountId", "99");
        assertThat(empty.get("rows")).isEmpty();
        assertThat(empty.get("hasNextPage").asBoolean()).isFalse();
        assertThat(empty.get("message").asText()).isEqualTo("NO MORE RECORDS TO SHOW");
        assertThat(empty.get("infoMessage").asText()).isEmpty();
    }

    @Test
    void R22_otherFileErrorsAbend() throws Exception {
        given(cards.findByCardNumGreaterThanEqualOrderByCardNumAsc(anyString(), any(Limit.class)))
                .willThrow(new DataAccessResourceFailureException("down"));
        list(admin()).andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.code").value("ABEND"))
                .andExpect(jsonPath("$.message").value(
                        "USER ABEND U0999: File Error: READ     on CARDDAT   returned RESP 000000017 ,RESP2 000000000"));
    }

    @Test
    void R23_pf7ReadsBackwardsFromTheFirstRow() throws Exception {
        JsonNode second = page(admin(), "after", page(admin()).get("nextPage").asText());
        JsonNode third = page(admin(), "after", second.get("nextPage").asText());
        JsonNode back = page(admin(), "before", third.get("previousPage").asText());
        assertThat(maskedOf(back)).isEqualTo(maskedRange(8, 14));
        assertThat(back.get("hasPreviousPage").asBoolean()).isTrue();
        assertThat(back.get("hasNextPage").asBoolean()).isTrue();
        JsonNode filtered = page(admin(), "accountId", "1", "before", refsOf(page(admin(), "accountId", "1",
                "after", refsOf(page(admin(), "accountId", "1")).get(6))).get(0));
        assertThat(maskedOf(filtered)).isEqualTo(maskedRange(1, 7));
    }

    @Test
    void R24_screenCarriesPageSizeAndCursors() throws Exception {
        list(admin()).andExpect(jsonPath("$.pageSize").value(7))
                .andExpect(jsonPath("$.previousPage").doesNotExist())
                .andExpect(jsonPath("$.nextPage").isString());
    }

    @Test
    void R25_rowsShowAccountMaskedCardAndStatus() throws Exception {
        list(admin()).andExpect(jsonPath("$.rows[4].row").value(5))
                .andExpect(jsonPath("$.rows[4].accountId").value("00000000001"))
                .andExpect(jsonPath("$.rows[4].cardNumber").value(masked(5)))
                .andExpect(jsonPath("$.rows[4].activeStatus").value("N"))
                .andExpect(jsonPath("$.rows[0].activeStatus").value("Y"));
    }

    @Test
    void R26_filtersAreRedisplayed() throws Exception {
        list(admin(), "accountId", "2", "cardNumber", pan(12)).andExpect(jsonPath("$.accountId").value("00000000002"))
                .andExpect(jsonPath("$.cardNumber").value(masked(12)));
    }

    @Test
    void R27_infoMessageWhenThereIsMoreToShow() throws Exception {
        list(admin()).andExpect(jsonPath("$.infoMessage").value("TYPE S FOR DETAIL, U TO UPDATE ANY RECORD"))
                .andExpect(jsonPath("$.message").value(""));
    }

    @Test
    void R28_theScreenIsSentAsJsonAndOtherMethodsAreRejected() throws Exception {
        list(admin()).andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON));
        mvc.perform(delete(CARDS).header(HttpHeaders.AUTHORIZATION, admin()))
                .andExpect(status().isMethodNotAllowed());
    }

    @Test
    void adminSeesAllCardsUserOnlyTheAccountInContext() throws Exception {
        assertThat(accountsOf(page(admin()))).hasSize(7);
        list(user()).andExpect(status().isForbidden())
                .andExpect(jsonPath("$.code").value("NOTAUTH"))
                .andExpect(jsonPath("$.field").value("accountId"))
                .andExpect(jsonPath("$.message").value(
                        "A regular user can only list the cards of the account in context: supply accountId"));
        list(user(), "cardNumber", pan(3)).andExpect(status().isForbidden());
        assertThat(accountsOf(page(user(), "accountId", "2"))).hasSize(7).containsOnly("00000000002");
    }

    @Test
    void listResponsesNeverCarryAFullCardNumber() throws Exception {
        String body = list(admin()).andReturn().getResponse().getContentAsString();
        for (int i = 1; i <= 7; i++) {
            assertThat(body).doesNotContain(pan(i)).contains(masked(i));
        }
    }

    @Test
    void cardRefIsAcceptedAsFilterAndCursorButTamperedRefsAreNot() throws Exception {
        String ref = refsOf(page(admin())).get(2);
        assertThat(maskedOf(page(admin(), "cardNumber", ref))).containsExactly(masked(3));
        assertThat(maskedOf(page(admin(), "after", pan(7)))).isEqualTo(maskedRange(8, 14));
        String tampered = ref.substring(0, ref.length() - 2) + (ref.endsWith("AA") ? "BB" : "AA");
        list(admin(), "after", tampered).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.field").value("after"));
    }

    @Test
    void onlySevenRowsAndOneCursorAreAccepted() throws Exception {
        list(admin(), "limit", "10").andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.field").value("limit"));
        list(admin(), "limit", "7").andExpect(status().isOk());
        list(admin(), "after", pan(1), "before", pan(9)).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.field").value("before"));
    }
}
