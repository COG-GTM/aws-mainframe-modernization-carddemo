package com.carddemo.web;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.BDDMockito.given;
import static org.hamcrest.Matchers.endsWith;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.patch;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.springframework.dao.DataAccessResourceFailureException;
import org.springframework.data.domain.Limit;
import org.springframework.http.HttpHeaders;

/** {@code docs/modernization/rules/COUSR00C.md} against {@code GET /api/v1/users} and {@code POST .../selection}. */
class UserListRulesTest extends UserWebTest {

    private static List<String> page1() {
        List<String> ids = new ArrayList<>(List.of(ADMIN));
        ids.addAll(users(1, 9));
        return ids;
    }

    private JsonNode page(String... params) throws Exception {
        return body(list(params).andExpect(status().isOk()));
    }

    @Test
    void R1_eachInvocationStartsWithABlankMessage() throws Exception {
        assertThat(page().get("message").asText()).isEmpty();
        assertThat(page("after", userId(9), "page", "1").get("message").asText()).isEmpty();
    }

    @Test
    void R2_noSessionReturnsToSignOn() throws Exception {
        mvc.perform(get(USERS)).andExpect(status().isUnauthorized())
                .andExpect(jsonPath("$.code").value("SIGNON_REQUIRED"))
                .andExpect(jsonPath("$.toProgram").value("COSGN00C"));
    }

    @Test
    void R3_firstEntryShowsPage1FromTheLowestKey() throws Exception {
        JsonNode first = page();
        assertThat(first.get("pageNumber").asInt()).isEqualTo(1);
        assertThat(first.get("pageSize").asInt()).isEqualTo(10);
        assertThat(idsOf(first)).isEqualTo(page1());
    }

    @Test
    void R4_enterRelistsFromTheTypedUserId() throws Exception {
        assertThat(idsOf(page("startUserId", userId(5)))).isEqualTo(users(5, 14));
    }

    @Test
    void R5_pf3ReturnsToTheAdminMenu() throws Exception {
        list().andExpect(jsonPath("$.exit.toProgram").value("COADM01C"))
                .andExpect(jsonPath("$.exit.toTranId").value("CA00"));
    }

    @Test
    void R6_pf7PagesBack() throws Exception {
        JsonNode second = page("after", userId(9), "page", "1");
        JsonNode back = page("before", second.get("previousPage").asText(), "page", "2");
        assertThat(idsOf(back)).isEqualTo(page1());
        assertThat(back.get("pageNumber").asInt()).isEqualTo(1);
    }

    @Test
    void R7_pf8PagesForward() throws Exception {
        JsonNode first = page();
        JsonNode second = page("after", first.get("nextPage").asText(), "page", "1");
        assertThat(idsOf(second)).isEqualTo(users(10, 19));
        assertThat(second.get("pageNumber").asInt()).isEqualTo(2);
    }

    @Test
    void R8_otherKeysAreInvalid() throws Exception {
        mvc.perform(patch(USERS).header(HttpHeaders.AUTHORIZATION, admin()))
                .andExpect(status().isMethodNotAllowed()).andExpect(jsonPath("$.code").value("INVALID_KEY"));
    }

    @Test
    void R9_onlyTheFirstMarkedRowCounts() throws Exception {
        select(users(1, 3), List.of("", "D", "U")).andExpect(status().isOk())
                .andExpect(jsonPath("$.userId").value(userId(2)))
                .andExpect(jsonPath("$.navigation.toProgram").value("COUSR03C"));
        select(users(1, 2), List.of("", " ")).andExpect(status().isOk())
                .andExpect(jsonPath("$.navigation").doesNotExist()).andExpect(jsonPath("$.userId").doesNotExist());
    }

    @Test
    void R10_uOrLowerUTransfersToCousr02c() throws Exception {
        for (String code : List.of("U", "u")) {
            select(List.of(userId(3)), List.of(code)).andExpect(status().isOk())
                    .andExpect(jsonPath("$.navigation.toProgram").value("COUSR02C"))
                    .andExpect(jsonPath("$.navigation.toTranId").value("CU02"))
                    .andExpect(jsonPath("$.navigation.fromProgram").value("COUSR00C"))
                    .andExpect(jsonPath("$.navigation.fromTranId").value("CU00"))
                    .andExpect(jsonPath("$.next").value("GET /api/v1/users/USER0003?fromProgram=COUSR00C"));
        }
    }

    @Test
    void selectionKeepsLeadingSpacesOfTheIdAndEncodesItInTheNextRequest() throws Exception {
        select(List.of(" A123   "), List.of("U")).andExpect(status().isOk())
                .andExpect(jsonPath("$.userId").value(" A123"))
                .andExpect(jsonPath("$.next").value("GET /api/v1/users/%20A123?fromProgram=COUSR00C"));
    }

    @Test
    void R11_dOrLowerDTransfersToCousr03c() throws Exception {
        for (String code : List.of("D", "d")) {
            select(List.of(userId(4)), List.of(code)).andExpect(status().isOk())
                    .andExpect(jsonPath("$.navigation.toProgram").value("COUSR03C"))
                    .andExpect(jsonPath("$.navigation.toTranId").value("CU03"))
                    .andExpect(jsonPath("$.next").value("DELETE /api/v1/users/USER0004?fromProgram=COUSR00C"));
        }
    }

    @Test
    void R12_anyOtherCodeIsAnInvalidSelection() throws Exception {
        select(users(1, 2), List.of("", "X")).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.code").value("INVREQ"))
                .andExpect(jsonPath("$.field").value("rows[1].selection"))
                .andExpect(jsonPath("$.message").value("Invalid selection. Valid values are U and D"));
    }

    @Test
    void R13_theStartKeyPositionsByPrefixWithoutValidation() throws Exception {
        assertThat(idsOf(page("startUserId", "USER001"))).first().isEqualTo(userId(10));
        assertThat(idsOf(page("startUserId", "   "))).isEqualTo(page1());
        assertThat(idsOf(page("startUserId", "B"))).first().isEqualTo(userId(1));
    }

    @Test
    void R14_pf7AfterPage1BrowsesBackFromTheFirstIdShown() throws Exception {
        JsonNode third = page("after", userId(19), "page", "2");
        JsonNode second = page("before", third.get("rows").get(0).get("userId").asText(), "page", "3");
        assertThat(idsOf(second)).isEqualTo(users(10, 19));
        assertThat(second.get("pageNumber").asInt()).isEqualTo(2);
        assertThat(second.get("hasNextPage").asBoolean()).isTrue();
    }

    @Test
    void R15_pf7OnPage1IsAlreadyAtTheTop() throws Exception {
        JsonNode same = page("before", ADMIN, "page", "1");
        assertThat(same.get("message").asText()).isEqualTo("You are already at the top of the page...");
        assertThat(idsOf(same)).isEqualTo(page1());
        assertThat(same.get("hasPreviousPage").asBoolean()).isFalse();
    }

    @Test
    void R16_pf8BrowsesOnFromTheLastIdShown() throws Exception {
        assertThat(idsOf(page("after", userId(19), "page", "2"))).isEqualTo(users(20, 22));
    }

    @Test
    void R17_pf8WithNoNextPageIsAlreadyAtTheBottom() throws Exception {
        JsonNode beyond = page("after", userId(22), "page", "3");
        assertThat(beyond.get("message").asText()).isEqualTo("You are already at the bottom of the page...");
        assertThat(beyond.get("hasNextPage").asBoolean()).isFalse();
    }

    @Test
    void R18_pf8SkipsTheLastRowOfThePreviousPage() throws Exception {
        assertThat(idsOf(page("after", userId(9), "page", "1"))).first().isEqualTo(userId(10));
    }

    @Test
    void R19_rowsCarryIdNamesAndType() throws Exception {
        JsonNode row = page().get("rows").get(1);
        assertThat(row.get("row").asInt()).isEqualTo(2);
        assertThat(row.get("userId").asText()).isEqualTo(userId(1));
        assertThat(row.get("firstName").asText()).isEqualTo("First1");
        assertThat(row.get("lastName").asText()).isEqualTo("Last1");
        assertThat(row.get("userType").asText()).isEqualTo("U");
        assertThat(page().get("rows").get(0).get("userType").asText()).isEqualTo("A");
        assertThat(row.has("password")).isFalse();
    }

    @Test
    void R20_aFullPageLooksAheadForANextPage() throws Exception {
        JsonNode first = page();
        assertThat(first.get("hasNextPage").asBoolean()).isTrue();
        assertThat(first.get("nextPage").asText()).isEqualTo(userId(9));
        store.headMap(userId(10)).clear();
        store.tailMap(userId(10), true).keySet().removeIf(k -> k.compareTo(userId(19)) > 0);
        JsonNode exactlyTen = page();
        assertThat(idsOf(exactlyTen)).hasSize(10);
        assertThat(exactlyTen.get("hasNextPage").asBoolean()).isFalse();
        assertThat(exactlyTen.get("nextPage").isNull()).isTrue();
    }

    @Test
    void R21_aShortPageHasNoNextPage() throws Exception {
        JsonNode last = page("after", userId(19), "page", "2");
        assertThat(last.get("rows")).hasSize(3);
        assertThat(last.get("hasNextPage").asBoolean()).isFalse();
        assertThat(last.get("pageNumber").asInt()).isEqualTo(3);
    }

    @Test
    void R22_thePageNumberIsReturned() throws Exception {
        assertThat(page().get("pageNumber").asInt()).isEqualTo(1);
        assertThat(page("after", userId(9), "page", "1").get("pageNumber").asInt()).isEqualTo(2);
    }

    @Test
    void R23_pf7EndsAtTheRecordBeforeTheOldFirstRow() throws Exception {
        JsonNode back = page("before", userId(15), "page", "3");
        assertThat(idsOf(back)).isEqualTo(users(5, 14));
        assertThat(back.get("hasPreviousPage").asBoolean()).isTrue();
    }

    @Test
    void R24_aStartKeyBeyondTheLastUserIsTheTopOfThePage() throws Exception {
        JsonNode none = page("startUserId", "ZZZZZZZZ");
        assertThat(none.get("rows")).isEmpty();
        assertThat(none.get("message").asText()).isEqualTo("You are at the top of the page...");
        given(users.findByUsrIdGreaterThanEqualOrderByUsrIdAsc(anyString(), any(Limit.class)))
                .willThrow(new DataAccessResourceFailureException("down"));
        list().andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.message", endsWith("Unable to lookup User...")));
    }

    @Test
    void R25_endOfFileForwardHasReachedTheBottom() throws Exception {
        assertThat(page("after", userId(19), "page", "2").get("message").asText())
                .isEqualTo("You have reached the bottom of the page...");
        given(users.findByUsrIdGreaterThanOrderByUsrIdAsc(anyString(), any(Limit.class)))
                .willThrow(new DataAccessResourceFailureException("down"));
        list("after", userId(9)).andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.message", endsWith("Unable to lookup User...")));
    }

    @Test
    void R26_endOfFileBackwardHasReachedTheTop() throws Exception {
        assertThat(page("before", userId(10), "page", "2").get("message").asText())
                .isEqualTo("You have reached the top of the page...");
        given(users.findByUsrIdLessThanOrderByUsrIdDesc(anyString(), any(Limit.class)))
                .willThrow(new DataAccessResourceFailureException("down"));
        list("before", userId(10), "page", "2").andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.message", endsWith("Unable to lookup User...")));
    }

    @Test
    void R27_returnCarriesTheFromFields() throws Exception {
        list().andExpect(jsonPath("$.exit.fromTranId").value("CU00"))
                .andExpect(jsonPath("$.exit.fromProgram").value("COUSR00C"));
    }

    @Test
    void R28_theScreenHeaderNamesCu00() throws Exception {
        list().andExpect(jsonPath("$.header.tranId").value("CU00"))
                .andExpect(jsonPath("$.header.programName").value("COUSR00C"))
                .andExpect(jsonPath("$.header.currentDate").value("07/06/22"));
    }

    @Test
    void cursorsAndLimitAreValidated() throws Exception {
        list("after", userId(1), "before", userId(5)).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.field").value("before"));
        list("limit", "20").andExpect(status().isBadRequest()).andExpect(jsonPath("$.field").value("limit"));
        list("after", "TOOLONGID").andExpect(status().isBadRequest()).andExpect(jsonPath("$.field").value("after"));
    }
}
