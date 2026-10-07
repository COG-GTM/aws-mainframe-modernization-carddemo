package com.carddemo.web;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.springframework.dao.DataAccessResourceFailureException;
import org.springframework.data.domain.Limit;
import org.springframework.http.HttpHeaders;

/**
 * {@code docs/modernization/rules/COTRN00C.md} against {@code GET /api/v1/transactions} and
 * {@code POST /api/v1/transactions/selection}: one test per rule, R-id in the name.
 */
class TransactionListRulesTest extends TransactionWebTest {

    @Test
    void R1_noSessionIsRejected() throws Exception {
        mvc.perform(get(TRANSACTIONS)).andExpect(status().isUnauthorized());
    }

    @Test
    void R2_firstEntryListsTheFirstPage() throws Exception {
        JsonNode first = page();
        assertThat(idsOf(first)).isEqualTo(ids(1, 10));
        assertThat(first.get("pageNumber").asInt()).isEqualTo(1);
        assertThat(first.get("message").asText()).isEmpty();
    }

    @Test
    void R3_enterPf7AndPf8AreStartTranIdBeforeAndAfter() throws Exception {
        assertThat(idsOf(page("startTranId", "12"))).isEqualTo(ids(12, 21));
        assertThat(idsOf(page("after", id(10), "page", "1"))).isEqualTo(ids(11, 20));
        assertThat(idsOf(page("before", id(11), "page", "2"))).isEqualTo(ids(1, 10));
        list(user(), "after", id(10), "before", id(11)).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.field").value("before"));
        list(user(), "limit", "20").andExpect(status().isBadRequest()).andExpect(jsonPath("$.field").value("limit"));
        list(user(), "limit", "10").andExpect(status().isOk());
        list(user(), "after", "x").andExpect(status().isBadRequest()).andExpect(jsonPath("$.field").value("after"));
    }

    @Test
    void R4_pf3ReturnsToTheMainMenu() throws Exception {
        list(user()).andExpect(jsonPath("$.exit.fromTranId").value("CT00"))
                .andExpect(jsonPath("$.exit.fromProgram").value("COTRN00C"))
                .andExpect(jsonPath("$.exit.toTranId").value("CM00"))
                .andExpect(jsonPath("$.exit.toProgram").value("COMEN01C"));
    }

    @Test
    void R5_otherKeysAreInvalid() throws Exception {
        mvc.perform(delete(TRANSACTIONS).header(HttpHeaders.AUTHORIZATION, user()))
                .andExpect(status().isMethodNotAllowed()).andExpect(jsonPath("$.code").value("INVALID_KEY"));
    }

    @Test
    void R6_theFirstNonBlankSelectionDecides() throws Exception {
        select(List.of(id(1), id(2), id(3)), List.of("", "S", "X")).andExpect(status().isOk())
                .andExpect(jsonPath("$.tranId").value(id(2)));
        select(List.of(id(1), id(2)), List.of("", " ")).andExpect(status().isOk())
                .andExpect(jsonPath("$.navigation").doesNotExist())
                .andExpect(jsonPath("$.tranId").doesNotExist());
    }

    @Test
    void R7_selectionSTransfersToTheDetail() throws Exception {
        for (String code : List.of("S", "s")) {
            select(List.of(id(1), id(4)), List.of("", code)).andExpect(status().isOk())
                    .andExpect(jsonPath("$.navigation.fromTranId").value("CT00"))
                    .andExpect(jsonPath("$.navigation.fromProgram").value("COTRN00C"))
                    .andExpect(jsonPath("$.navigation.toTranId").value("CT01"))
                    .andExpect(jsonPath("$.navigation.toProgram").value("COTRN01C"))
                    .andExpect(jsonPath("$.tranId").value(id(4)))
                    .andExpect(jsonPath("$.next").value(TRANSACTIONS + "/" + id(4) + "?fromProgram=COTRN00C"));
        }
    }

    @Test
    void R8_anyOtherSelectionCodeIsInvalid() throws Exception {
        select(List.of(id(1), id(2)), List.of("", "U")).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.code").value("INVREQ"))
                .andExpect(jsonPath("$.field").value("rows[1].selection"))
                .andExpect(jsonPath("$.message").value("Invalid selection. Valid value is S"));
    }

    @Test
    void R9_blankStartIdBrowsesFromLowValues() throws Exception {
        assertThat(idsOf(page("startTranId", "   "))).isEqualTo(ids(1, 10));
    }

    @Test
    void R10_numericStartIdPositionsExactOrGreater() throws Exception {
        assertThat(idsOf(page("startTranId", id(5))).get(0)).isEqualTo(id(5));
        store.remove(id(7));
        assertThat(idsOf(page("startTranId", "7")).get(0)).isEqualTo(id(8));
    }

    @Test
    void R11_nonNumericStartIdIsRejected() throws Exception {
        list(user(), "startTranId", "12AB").andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.field").value("tranId"))
                .andExpect(jsonPath("$.message").value("Tran ID must be Numeric ..."));
    }

    @Test
    void R12_enterRestartsThePageCount() throws Exception {
        assertThat(page("startTranId", "15").get("pageNumber").asInt()).isEqualTo(1);
    }

    @Test
    void R13_pf7AfterPageOneReadsBackward() throws Exception {
        JsonNode third = page("after", id(20), "page", "2");
        JsonNode back = page("before", third.get("previousPage").asText(), "page", "3");
        assertThat(idsOf(back)).isEqualTo(ids(11, 20));
        assertThat(back.get("pageNumber").asInt()).isEqualTo(2);
        assertThat(back.get("hasNextPage").asBoolean()).isTrue();
    }

    @Test
    void R14_pf7OnPageOneIsAlreadyAtTheTop() throws Exception {
        JsonNode top = page("before", id(1), "page", "1");
        assertThat(top.get("message").asText()).isEqualTo("You are already at the top of the page...");
        assertThat(idsOf(top)).isEqualTo(ids(1, 10));
        assertThat(top.get("hasPreviousPage").asBoolean()).isFalse();
    }

    @Test
    void R15_pf8WithANextPageReadsForward() throws Exception {
        JsonNode first = page();
        assertThat(first.get("hasNextPage").asBoolean()).isTrue();
        JsonNode second = page("after", first.get("nextPage").asText(), "page", "1");
        assertThat(idsOf(second)).isEqualTo(ids(11, 20));
        assertThat(second.get("pageNumber").asInt()).isEqualTo(2);
    }

    @Test
    void R16_pf8WithoutANextPageIsAlreadyAtTheBottom() throws Exception {
        JsonNode bottom = page("after", id(23), "page", "3");
        assertThat(bottom.get("message").asText()).isEqualTo("You are already at the bottom of the page...");
        assertThat(bottom.get("hasNextPage").asBoolean()).isFalse();
        assertThat(bottom.get("rows")).isEmpty();
    }

    @Test
    void R17_rowsShowIdDateTruncatedDescriptionAndEditedAmount() throws Exception {
        JsonNode first = page();
        JsonNode row1 = first.get("rows").get(0);
        assertThat(row1.get("row").asInt()).isEqualTo(1);
        assertThat(row1.get("tranId").asText()).isEqualTo(id(1));
        assertThat(row1.get("date").asText()).isEqualTo("06/01/22");
        assertThat(row1.get("description").asText()).isEqualTo(LONG_DESCRIPTION.substring(0, 26));
        assertThat(row1.get("amount").asText()).isEqualTo("+00000045.10");
        assertThat(first.get("rows").get(1).get("amount").asText()).isEqualTo("-00000120.00");
        assertThat(first.get("nextPage").asText()).isEqualTo(id(10));
        assertThat(first.get("rows").get(0).has("cardNumber")).isFalse();
    }

    @Test
    void R18_startbrNotFoundAndOtherErrors() throws Exception {
        store.clear();
        JsonNode empty = page();
        assertThat(empty.get("rows")).isEmpty();
        assertThat(empty.get("message").asText()).isEqualTo("You are at the top of the page...");
        given(transactions.findByTranIdGreaterThanEqualOrderByTranIdAsc(anyString(), any(Limit.class)))
                .willThrow(new DataAccessResourceFailureException("down"));
        list(user()).andExpect(status().isInternalServerError()).andExpect(jsonPath("$.code").value("ABEND"))
                .andExpect(jsonPath("$.message", endsWith("Unable to lookup transaction...")));
    }

    @Test
    void R19_readnextEndfileIsTheBottom() throws Exception {
        JsonNode last = page("after", id(20), "page", "2");
        assertThat(idsOf(last)).isEqualTo(ids(21, 23));
        assertThat(last.get("message").asText()).isEqualTo("You have reached the bottom of the page...");
        assertThat(last.get("nextPage").isNull()).isTrue();
    }

    @Test
    void R20_readprevEndfileIsTheTop() throws Exception {
        JsonNode back = page("before", id(11), "page", "2");
        assertThat(idsOf(back)).isEqualTo(ids(1, 10));
        assertThat(back.get("message").asText()).isEqualTo("You have reached the top of the page...");
        assertThat(back.get("hasPreviousPage").asBoolean()).isFalse();
    }

    @Test
    void R21_pagesOfTenWithTheCounterIncrementingPerFilledPage() throws Exception {
        JsonNode page = page();
        List<Integer> sizes = new ArrayList<>();
        List<Integer> numbers = new ArrayList<>();
        while (true) {
            sizes.add(page.get("rows").size());
            numbers.add(page.get("pageNumber").asInt());
            if (!page.get("hasNextPage").asBoolean()) {
                break;
            }
            page = page("after", page.get("nextPage").asText(), "page", page.get("pageNumber").asText());
        }
        assertThat(sizes).containsExactly(10, 10, 3);
        assertThat(numbers).containsExactly(1, 2, 3);
        assertThat(page("after", id(23), "page", "3").get("pageNumber").asInt()).isEqualTo(3);
    }

    @Test
    void R22_returnWithoutSessionIsSignOnAndCarriesTheFromFields() throws Exception {
        mvc.perform(get(TRANSACTIONS)).andExpect(status().isUnauthorized());
        list(user()).andExpect(jsonPath("$.exit.fromTranId").value("CT00"))
                .andExpect(jsonPath("$.exit.fromProgram").value("COTRN00C"));
    }

    @Test
    void R23_standardHeader() throws Exception {
        list(user()).andExpect(jsonPath("$.header.tranId").value("CT00"))
                .andExpect(jsonPath("$.header.programName").value("COTRN00C"))
                .andExpect(jsonPath("$.header.currentDate").value("07/06/22"))
                .andExpect(jsonPath("$.header.applId").value("CARDDEMO"))
                .andExpect(jsonPath("$.header.sysId").value("CDMO"));
    }
}
