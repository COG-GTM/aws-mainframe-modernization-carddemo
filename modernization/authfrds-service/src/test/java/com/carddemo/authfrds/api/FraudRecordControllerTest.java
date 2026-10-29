package com.carddemo.authfrds.api;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.authfrds.adapters.ims.InMemoryPendingAuthorizationStore;
import com.carddemo.authfrds.domain.AuthorizationFraudService;
import com.carddemo.authfrds.domain.Fixtures;
import com.carddemo.authfrds.domain.FraudRecordService;
import com.carddemo.authfrds.domain.FraudRecordStore;
import com.carddemo.authfrds.domain.InMemoryFraudRecordStore;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Import;
import org.springframework.http.MediaType;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.web.servlet.MockMvc;

@WebMvcTest({FraudRecordController.class, AuthorizationController.class})
@Import({FraudRecordService.class, AuthorizationFraudService.class, InMemoryPendingAuthorizationStore.class,
        FraudRecordControllerTest.Config.class})
@DirtiesContext(classMode = DirtiesContext.ClassMode.AFTER_EACH_TEST_METHOD)
class FraudRecordControllerTest {

    @TestConfiguration
    static class Config {
        @Bean
        FraudRecordStore store() {
            return new InMemoryFraudRecordStore();
        }

        @Bean
        @org.springframework.context.annotation.Primary
        Clock fixedClock() {
            return Clock.fixed(Instant.parse("2026-10-29T12:00:00Z"), ZoneOffset.UTC);
        }
    }

    @Autowired
    MockMvc mvc;
    @Autowired
    ObjectMapper json;
    static final String CARD = "4111222233334444";

    private String body(String card, String action) throws Exception {
        return json.writeValueAsString(Fixtures.request(card, action));
    }

    @Test
    @DisplayName("BR-AF-004 POST first time -> 201 ADD SUCCESS")
    void add() throws Exception {
        mvc.perform(post("/api/v1/fraud-records").contentType(MediaType.APPLICATION_JSON).content(body(CARD, "F")))
                .andExpect(status().isCreated()).andExpect(jsonPath("$.message").value("ADD SUCCESS"))
                .andExpect(jsonPath("$.authTs").value("26-10-29 15.00.00122000"));
    }

    @Test
    @DisplayName("BR-AF-006 POST same key again -> 200 UPDT SUCCESS")
    void updt() throws Exception {
        mvc.perform(post("/api/v1/fraud-records").contentType(MediaType.APPLICATION_JSON).content(body(CARD, "F")));
        mvc.perform(post("/api/v1/fraud-records").contentType(MediaType.APPLICATION_JSON).content(body(CARD, "R")))
                .andExpect(status().isOk()).andExpect(jsonPath("$.message").value("UPDT SUCCESS"))
                .andExpect(jsonPath("$.record.authFraud").value("R"));
    }

    @Test
    @DisplayName("BR-AF-011 request that does not fit the commarea PICs -> 400 with field errors, no SQL")
    void validation() throws Exception {
        String bad = body("4111", "X").replace("\"261029\"", "\"2610\"");
        mvc.perform(post("/api/v1/fraud-records").contentType(MediaType.APPLICATION_JSON).content(bad))
                .andExpect(status().isBadRequest()).andExpect(jsonPath("$.ruleId").value("BR-AF-011"))
                .andExpect(jsonPath("$.fields.length()").value(3));
    }

    @Test
    @DisplayName("BR-AF-030 GET list pages in XAUTHFRD order (CARD_NUM ASC, AUTH_TS DESC), 5 per page")
    void paging() throws Exception {
        for (int i = 0; i < 7; i++) {
            mvc.perform(post("/api/v1/fraud-records").contentType(MediaType.APPLICATION_JSON)
                    .content(body("41112222333300" + (10 + i), "F")));
        }
        mvc.perform(get("/api/v1/fraud-records")).andExpect(status().isOk())
                .andExpect(jsonPath("$.items.length()").value(5)).andExpect(jsonPath("$.total").value(7))
                .andExpect(jsonPath("$.hasNext").value(true)).andExpect(jsonPath("$.items[0].cardNum").value("4111222233330010"));
        mvc.perform(get("/api/v1/fraud-records?page=1")).andExpect(jsonPath("$.items.length()").value(2))
                .andExpect(jsonPath("$.hasPrevious").value(true)).andExpect(jsonPath("$.hasNext").value(false));
        mvc.perform(get("/api/v1/fraud-records?cardNum=4111222233330012")).andExpect(jsonPath("$.total").value(1));
    }

    @Test
    @DisplayName("BR-AF-007 GET a key with no row -> 404 SQLCODE +100")
    void notFound() throws Exception {
        mvc.perform(get("/api/v1/fraud-records/" + CARD + "/2026-10-29T15:00:00.122"))
                .andExpect(status().isNotFound()).andExpect(jsonPath("$.sqlCode").value(100));
    }

    @Test
    @DisplayName("BR-AF-022 PF5 toggle -> 200 'AUTH MARKED FRAUD...', then 'AUTH FRAUD REMOVED...'")
    void toggle() throws Exception {
        String key = "/api/v1/accounts/1/authorizations/";
        String first = json.readTree(mvc.perform(get(key.substring(0, key.length() - 1)))
                .andReturn().getResponse().getContentAsString()).get(0).get("authKey").asText();
        mvc.perform(post(key + first + "/fraud")).andExpect(status().isOk())
                .andExpect(jsonPath("$.message").value("AUTH MARKED FRAUD..."));
        mvc.perform(post(key + first + "/fraud")).andExpect(jsonPath("$.message").value("AUTH FRAUD REMOVED..."));
    }

    @Test
    @DisplayName("BR-AF-024 IMS REPL failure -> 500 with the ROLLBACK message")
    void rollback() throws Exception {
        String first = json.readTree(mvc.perform(get("/api/v1/accounts/1/authorizations"))
                .andReturn().getResponse().getContentAsString()).get(0).get("authKey").asText();
        mvc.perform(post("/api/v1/ims-stub/fail-next-repl?status=AI"));
        mvc.perform(post("/api/v1/accounts/1/authorizations/" + first + "/fraud"))
                .andExpect(status().isInternalServerError())
                .andExpect(jsonPath("$.message").value(" System error while FRAUD Tagging, ROLLBACK||AI"));
    }

    @Test
    @DisplayName("BR-AF-027 PF8 past the last authorization -> 404 'Already at the last Authorization...'")
    void lastAuth() throws Exception {
        var list = json.readTree(mvc.perform(get("/api/v1/accounts/1/authorizations"))
                .andReturn().getResponse().getContentAsString());
        String last = list.get(list.size() - 1).get("authKey").asText();
        mvc.perform(get("/api/v1/accounts/1/authorizations/" + last + "/next")).andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("Already at the last Authorization..."));
    }
}
