package com.carddemo;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.HashMap;
import java.util.Map;
import org.junit.jupiter.api.Test;

class CardIntegrationTest extends IntegrationTestBase {

    @Test
    void listPagesSevenCardsLikeCocrdlic() throws Exception {
        String token = userToken();
        JsonNode first = body(mvc.perform(as(token, get("/api/v1/cards")))
                .andExpect(status().isOk()).andReturn());
        assertThat(first.get("items")).hasSize(7);
        assertThat(first.get("hasNext").asBoolean()).isTrue();
        assertThat(first.get("hasPrev").asBoolean()).isFalse();

        JsonNode second = body(mvc.perform(as(token, get("/api/v1/cards")
                .param("startKey", first.get("lastKey").asText()).param("direction", "next")))
                .andExpect(status().isOk()).andReturn());
        assertThat(second.get("items")).hasSize(7);
        assertThat(second.get("hasPrev").asBoolean()).isTrue();
        assertThat(second.get("firstKey").asText()).isGreaterThan(first.get("lastKey").asText());

        JsonNode back = body(mvc.perform(as(token, get("/api/v1/cards")
                .param("startKey", second.get("firstKey").asText()).param("direction", "prev")))
                .andExpect(status().isOk()).andReturn());
        assertThat(back.get("items")).isEqualTo(first.get("items"));
    }

    @Test
    void listFiltersByAccount() throws Exception {
        mvc.perform(as(userToken(), get("/api/v1/cards").param("acctId", "00000000050")))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.items.length()").value(1))
                .andExpect(jsonPath("$.items[0].cardNum").value("0500024453765740"));
    }

    @Test
    void listRejectsInvalidFilter() throws Exception {
        mvc.perform(as(userToken(), get("/api/v1/cards").param("acctId", "12AB")))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER"));
    }

    @Test
    void listReportsNoMatches() throws Exception {
        mvc.perform(as(userToken(), get("/api/v1/cards").param("cardNum", "9999999999999999")))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.items.length()").value(0))
                .andExpect(jsonPath("$.message").value("NO RECORDS FOUND FOR THIS SEARCH CONDITION."));
    }

    @Test
    void detailReturnsCard() throws Exception {
        mvc.perform(as(userToken(), get("/api/v1/cards/0500024453765740")))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.acctId").value(50))
                .andExpect(jsonPath("$.embossedName").value("Aniya Von"))
                .andExpect(jsonPath("$.expirationDate").value("2023-03-09"))
                .andExpect(jsonPath("$.activeStatus").value("Y"));
    }

    @Test
    void detailValidatesAndReportsMissingCard() throws Exception {
        String token = userToken();
        mvc.perform(as(token, get("/api/v1/cards/12345")))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Card number if supplied must be a 16 digit number"));
        mvc.perform(as(token, get("/api/v1/cards/1111111111111111")))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("Did not find cards for this search condition"));
    }

    private Map<String, Object> update(String name, String status, String expiry, long version) {
        Map<String, Object> body = new HashMap<>();
        body.put("acctId", "00000000050");
        body.put("embossedName", name);
        body.put("activeStatus", status);
        body.put("expirationDate", expiry);
        body.put("version", version);
        return body;
    }

    @Test
    void updateChangesCardAndBumpsVersion() throws Exception {
        mvc.perform(as(userToken(), withJson(put("/api/v1/cards/0500024453765740"),
                update("Aniya Q Von", "N", "2027-11-30", 0))))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.embossedName").value("Aniya Q Von"))
                .andExpect(jsonPath("$.activeStatus").value("N"))
                .andExpect(jsonPath("$.expirationDate").value("2027-11-09"))
                .andExpect(jsonPath("$.version").value(1));
    }

    @Test
    void updateValidatesFields() throws Exception {
        JsonNode error = body(mvc.perform(as(userToken(), withJson(put("/api/v1/cards/0500024453765740"),
                update("Aniya 2", "X", "2027-13-01", 0))))
                .andExpect(status().isBadRequest()).andReturn());
        assertThat(error.get("fieldErrors").findValuesAsText("message")).contains(
                "Card name can only contain alphabets and spaces",
                "Card Active Status must be Y or N",
                "Card expiry month must be between 1 and 12");
    }

    @Test
    void updateWithStaleVersionIsConflict() throws Exception {
        String token = userToken();
        mvc.perform(as(token, withJson(put("/api/v1/cards/0500024453765740"),
                update("Aniya Q Von", "Y", "2027-11-30", 0)))).andExpect(status().isOk());
        mvc.perform(as(token, withJson(put("/api/v1/cards/0500024453765740"),
                update("Aniya R Von", "Y", "2027-11-30", 0))))
                .andExpect(status().isConflict())
                .andExpect(jsonPath("$.errorCode").value("CONCURRENT_UPDATE"));
    }

    @Test
    void updateWithoutChangesIsRejected() throws Exception {
        mvc.perform(as(userToken(), withJson(put("/api/v1/cards/0500024453765740"),
                update("Aniya Von", "Y", "2023-03-09", 0))))
                .andExpect(status().isUnprocessableEntity())
                .andExpect(jsonPath("$.message").value("No change detected with respect to values fetched."));
    }
}
