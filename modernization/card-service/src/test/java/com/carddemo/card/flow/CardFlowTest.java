package com.carddemo.card.flow;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.testsupport.flow.CobolFlow;
import com.carddemo.testsupport.flow.ScreenKey;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.LinkedHashMap;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.ResultActions;

/**
 * CCLI card list (COCRDLIC: 7 rows, account and card filters, PF7/PF8), CCDL card view (COCRDSLC) and
 * CCUP card update (COCRDUPC: ENTER fetches, PF5 validates and rewrites) on the sample cards. Expected
 * values are the sample CARDDATA records, e.g. {@code 9680294154603697} = account 1, "Immanuel Kessler",
 * 2025-05-20, active.
 */
class CardFlowTest extends CardFlowTestSupport {

    private ResultActions list(String... params) throws Exception {
        var request = get("/api/v1/cards");
        for (int i = 0; i < params.length; i += 2) {
            request.param(params[i], params[i + 1]);
        }
        return mockMvc.perform(asUser(request));
    }

    private JsonNode card(String cardNumber) throws Exception {
        return JSON.readTree(mockMvc.perform(asUser(get("/api/v1/cards/" + cardNumber)))
                .andExpect(status().isOk()).andReturn().getResponse().getContentAsString());
    }

    private ResultActions update(String cardNumber, Map<String, Object> body) throws Exception {
        return mockMvc.perform(asUser(put("/api/v1/cards/" + cardNumber).contentType(MediaType.APPLICATION_JSON)
                .content(JSON.writeValueAsString(body))));
    }

    /** The CCUP screen as COCRDUPC shows it after ENTER, i.e. the fetched card's editable fields. */
    private Map<String, Object> screen(String cardNumber) throws Exception {
        JsonNode card = card(cardNumber);
        String expiry = card.get("expirationDate").asText();
        Map<String, Object> body = new LinkedHashMap<>();
        body.put("embossedName", card.get("embossedName").asText());
        body.put("activeStatus", card.get("activeStatus").asText());
        body.put("expirationMonth", Integer.parseInt(expiry.substring(5, 7)));
        body.put("expirationYear", Integer.parseInt(expiry.substring(0, 4)));
        body.put("version", card.get("version").asLong());
        return body;
    }

    // ---- COCRDLIC -------------------------------------------------------------------------------

    @Test
    @CobolFlow(program = "COCRDLIC", paragraph = "9000-READ-FORWARD", key = ScreenKey.ENTER)
    void COCRDLIC_9000_READ_FORWARD_firstScreenShowsSevenCardsInCardNumberOrder() throws Exception {
        list("size", "7")
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content.length()").value(7))
                .andExpect(jsonPath("$.totalElements").value(50))
                .andExpect(jsonPath("$.totalPages").value(8))
                .andExpect(jsonPath("$.content[0].cardNumber").value("0500024453765740"))
                .andExpect(jsonPath("$.content[0].accountId").value("00000000050"))
                .andExpect(jsonPath("$.content[0].embossedName").value("Aniya Von"))
                .andExpect(jsonPath("$.content[0].activeStatus").value("Y"))
                .andExpect(jsonPath("$.content[6].cardNumber").value("1142167692878931"))
                .andExpect(jsonPath("$.content[0].cvv").doesNotExist());
    }

    @Test
    @CobolFlow(program = "COCRDLIC", paragraph = "9000-READ-FORWARD", key = ScreenKey.PF8)
    void COCRDLIC_9000_READ_FORWARD_pf8ShowsTheNextSevenAndTheLastScreenHasOne() throws Exception {
        list("size", "7", "page", "1")
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content.length()").value(7))
                .andExpect(jsonPath("$.content[0].cardNumber").value("1561409106491600"));
        list("size", "7", "page", "7")
                .andExpect(jsonPath("$.content.length()").value(1))
                .andExpect(jsonPath("$.content[0].cardNumber").value("9805583408996588"));
        // NO MORE PAGES TO DISPLAY: an empty page.
        list("size", "7", "page", "8").andExpect(jsonPath("$.content.length()").value(0));
    }

    @Test
    @CobolFlow(program = "COCRDLIC", paragraph = "9100-READ-BACKWARDS", key = ScreenKey.PF7)
    void COCRDLIC_9100_READ_BACKWARDS_pf7FromTheSecondScreenShowsTheFirstAgain() throws Exception {
        list("size", "7", "page", "0")
                .andExpect(jsonPath("$.content[0].cardNumber").value("0500024453765740"))
                .andExpect(jsonPath("$.content[6].cardNumber").value("1142167692878931"));
        list("size", "7", "page", "-1").andExpect(status().isBadRequest());
    }

    @Test
    @CobolFlow(program = "COCRDLIC", paragraph = "9500-FILTER-RECORDS", key = ScreenKey.ENTER)
    void COCRDLIC_9500_FILTER_RECORDS_accountFilterShowsThatAccountsCards() throws Exception {
        list("size", "7", "accountId", "00000000001")
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.totalElements").value(1))
                .andExpect(jsonPath("$.content[0].cardNumber").value("9680294154603697"))
                .andExpect(jsonPath("$.content[0].embossedName").value("Immanuel Kessler"));
    }

    @Test
    @CobolFlow(program = "COCRDLIC", paragraph = "9500-FILTER-RECORDS", key = ScreenKey.ENTER)
    void COCRDLIC_9500_FILTER_RECORDS_cardFilterAndBothFiltersIntersect() throws Exception {
        list("size", "7", "cardNumber", "0927987108636232")
                .andExpect(jsonPath("$.totalElements").value(1))
                .andExpect(jsonPath("$.content[0].embossedName").value("Carter Veum"));
        list("size", "7", "accountId", "00000000001", "cardNumber", "0927987108636232")
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.totalElements").value(0));
        // Blank or all-zero filters are "not supplied".
        list("size", "7", "accountId", "00000000000", "cardNumber", " ")
                .andExpect(jsonPath("$.totalElements").value(50));
    }

    @Test
    @CobolFlow(program = "COCRDLIC", paragraph = "2210-EDIT-ACCOUNT", key = ScreenKey.ENTER)
    void COCRDLIC_2210_EDIT_ACCOUNT_nonNumericAccountFilterIsRejected() throws Exception {
        list("accountId", "1234X")
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].message")
                        .value("ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER"));
    }

    @Test
    @CobolFlow(program = "COCRDLIC", paragraph = "2220-EDIT-CARD", key = ScreenKey.ENTER)
    void COCRDLIC_2220_EDIT_CARD_nonNumericCardFilterIsRejected() throws Exception {
        list("cardNumber", "06835861981715XX")
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].message")
                        .value("CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER"));
    }

    @Test
    @CobolFlow(program = "COCRDLIC", paragraph = "9000-READ-FORWARD", key = ScreenKey.ENTER)
    void COCRDLIC_9000_READ_FORWARD_userAndAdminSeeTheSameCards() throws Exception {
        String admin = "Bearer " + tokens.issue("ADMIN001", "A").token();
        String asAdmin = mockMvc.perform(get("/api/v1/cards").param("size", "50")
                        .header("Authorization", admin)).andReturn().getResponse().getContentAsString();
        String asUser = list("size", "50").andReturn().getResponse().getContentAsString();
        org.assertj.core.api.Assertions.assertThat(asUser).isEqualTo(asAdmin);
    }

    // ---- COCRDSLC -------------------------------------------------------------------------------

    @Test
    @CobolFlow(program = "COCRDSLC", paragraph = "9100-GETCARD-BYACCTCARD", key = ScreenKey.ENTER)
    void COCRDSLC_9100_GETCARD_BYACCTCARD_knownCardIsShown() throws Exception {
        mockMvc.perform(asUser(get("/api/v1/cards/9680294154603697")))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.cardNumber").value("9680294154603697"))
                .andExpect(jsonPath("$.accountId").value("00000000001"))
                .andExpect(jsonPath("$.embossedName").value("Immanuel Kessler"))
                .andExpect(jsonPath("$.expirationDate").value("2025-05-20"))
                .andExpect(jsonPath("$.activeStatus").value("Y"))
                .andExpect(jsonPath("$.cvv").doesNotExist());
    }

    @Test
    @CobolFlow(program = "COCRDSLC", paragraph = "9100-GETCARD-BYACCTCARD", key = ScreenKey.ENTER)
    void COCRDSLC_9100_GETCARD_BYACCTCARD_unknownCardIsNotFound() throws Exception {
        mockMvc.perform(asUser(get("/api/v1/cards/1111222233334444")))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("Did not find cards for this search condition"));
    }

    @Test
    @CobolFlow(program = "COCRDSLC", paragraph = "2220-EDIT-CARD", key = ScreenKey.ENTER)
    void COCRDSLC_2220_EDIT_CARD_zeroAndNonNumericCardNumbersAreRejected() throws Exception {
        mockMvc.perform(asUser(get("/api/v1/cards/0000000000000000")))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].message").value("Card number not provided"));
        mockMvc.perform(asUser(get("/api/v1/cards/96802941546036XX")))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].message")
                        .value("CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER"));
    }

    // ---- COCRDUPC -------------------------------------------------------------------------------

    @Test
    @CobolFlow(program = "COCRDUPC", paragraph = "9100-GETCARD-BYACCTCARD", key = ScreenKey.ENTER)
    void COCRDUPC_9100_GETCARD_BYACCTCARD_enterFetchesTheCardToUpdate() throws Exception {
        Map<String, Object> screen = screen("0923877193247330");
        org.assertj.core.api.Assertions.assertThat(screen).containsEntry("embossedName", "Enrico Rosenbaum")
                .containsEntry("activeStatus", "Y").containsEntry("expirationMonth", 8)
                .containsEntry("expirationYear", 2024);
    }

    /** Each COCRDUPC edit 1230-1260 on its own, the other fields as fetched. */
    @Test
    @CobolFlow(program = "COCRDUPC", paragraph = "1200-EDIT-MAP-INPUTS", key = ScreenKey.PF5)
    void COCRDUPC_1200_EDIT_MAP_INPUTS_everyEditMessage() throws Exception {
        Object[][] cases = {
                {"1230-EDIT-NAME", "embossedName", " ", "Card name not provided"},
                {"1230-EDIT-NAME", "embossedName", "Enrico R0senbaum", "Card name can only contain alphabets and spaces"},
                {"1240-EDIT-CARDSTATUS", "activeStatus", "X", "Card Active Status must be Y or N"},
                {"1240-EDIT-CARDSTATUS", "activeStatus", "y", "Card Active Status must be Y or N"},
                {"1250-EDIT-EXPIRY-MON", "expirationMonth", 13, "Card expiry month must be between 1 and 12"},
                {"1250-EDIT-EXPIRY-MON", "expirationMonth", 0, "Card expiry month must be between 1 and 12"},
                {"1260-EDIT-EXPIRY-YEAR", "expirationYear", 2100, "Invalid card expiry year"},
                {"1260-EDIT-EXPIRY-YEAR", "expirationYear", 1949, "Invalid card expiry year"}};
        for (Object[] c : cases) {
            Map<String, Object> body = screen("0923877193247330");
            body.put((String) c[1], c[2]);
            update("0923877193247330", body)
                    .andExpect(status().isBadRequest())
                    .andExpect(jsonPath("$.fieldErrors[0].field").value(c[1]))
                    .andExpect(jsonPath("$.fieldErrors[0].message").value(c[3]));
        }
        org.assertj.core.api.Assertions.assertThat(card("0923877193247330").get("version").asLong()).isZero();
    }

    @Test
    @CobolFlow(program = "COCRDUPC", paragraph = "1200-EDIT-MAP-INPUTS", key = ScreenKey.PF5)
    void COCRDUPC_1200_EDIT_MAP_INPUTS_unchangedCardIsNotRewritten() throws Exception {
        Map<String, Object> body = screen("0927987108636232");
        body.put("embossedName", "CARTER VEUM");  // the COBOL compares UPPER-CASE(new) with UPPER-CASE(old)
        update("0927987108636232", body)
                .andExpect(status().isUnprocessableEntity())
                .andExpect(jsonPath("$.message").value("No change detected with respect to values fetched."));
    }

    @Test
    @CobolFlow(program = "COCRDUPC", paragraph = "9200-WRITE-PROCESSING", key = ScreenKey.PF5)
    void COCRDUPC_9200_WRITE_PROCESSING_changedCardIsRewrittenKeepingTheDay() throws Exception {
        Map<String, Object> body = screen("0683586198171516");
        body.put("embossedName", "Ward A Jones");
        body.put("activeStatus", "N");
        body.put("expirationMonth", 12);
        body.put("expirationYear", 2027);
        update("0683586198171516", body)
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.embossedName").value("Ward A Jones"))
                .andExpect(jsonPath("$.activeStatus").value("N"))
                .andExpect(jsonPath("$.expirationDate").value("2027-12-13"))
                .andExpect(jsonPath("$.accountId").value("00000000027"))
                .andExpect(jsonPath("$.version").value(1));
        org.assertj.core.api.Assertions.assertThat(card("0683586198171516").get("embossedName").asText())
                .isEqualTo("Ward A Jones");
    }

    @Test
    @CobolFlow(program = "COCRDUPC", paragraph = "9300-CHECK-CHANGE-IN-REC", key = ScreenKey.PF5)
    void COCRDUPC_9300_CHECK_CHANGE_IN_REC_cardChangedSinceFetchIsRefused() throws Exception {
        Map<String, Object> first = screen("0982496213629795");
        Map<String, Object> second = new LinkedHashMap<>(first);
        first.put("activeStatus", "N");
        update("0982496213629795", first).andExpect(status().isOk());
        second.put("expirationYear", 2030);
        update("0982496213629795", second)
                .andExpect(status().isConflict())
                .andExpect(jsonPath("$.message").value("Record changed by some one else. Please review"));
    }

    @Test
    @CobolFlow(program = "COCRDUPC", paragraph = "9100-GETCARD-BYACCTCARD", key = ScreenKey.PF5)
    void COCRDUPC_9100_GETCARD_BYACCTCARD_unknownCardIsNotFound() throws Exception {
        Map<String, Object> body = screen("0923877193247330");
        update("1111222233334444", body)
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("Did not find cards for this search condition"));
    }

    /**
     * Parity kept on purpose (card-service-review.md, architecture.md §6): 1230-EDIT-NAME runs on every
     * update, and the one sample name with an apostrophe fails it, so that card cannot change status
     * without a new name.
     */
    @Test
    @CobolFlow(program = "COCRDUPC", paragraph = "1230-EDIT-NAME", key = ScreenKey.PF5)
    void COCRDUPC_1230_EDIT_NAME_apostropheInTheSampleNameBlocksAStatusChange() throws Exception {
        Map<String, Object> body = screen("7026637615032277");
        org.assertj.core.api.Assertions.assertThat(body).containsEntry("embossedName", "Lucious O'Connell");
        body.put("activeStatus", "N");
        update("7026637615032277", body)
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].message").value("Card name can only contain alphabets and spaces"));
        body.put("embossedName", "Lucious OConnell");
        update("7026637615032277", body).andExpect(status().isOk());
    }
}
