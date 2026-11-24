package com.carddemo.transactiontype.api;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.patch;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.transactiontype.domain.InMemoryTransactionTypeStore;
import com.carddemo.transactiontype.domain.TransactionTypeService;
import com.carddemo.transactiontype.domain.TransactionTypeStore;
import com.carddemo.transactiontype.domain.batch.BatchMaintenanceService;
import static org.assertj.core.api.Assertions.assertThat;

import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Import;
import org.springframework.http.MediaType;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.web.servlet.MockMvc;
import com.carddemo.transactiontype.config.SecurityConfig;

@WebMvcTest(TransactionTypeController.class)
@Import({SecurityConfig.class, TransactionTypeService.class, BatchMaintenanceService.class, TransactionTypeControllerTest.Store.class})
@WithMockUser(roles = SecurityConfig.MAINTAINER)
class TransactionTypeControllerTest {

    @TestConfiguration
    static class Store {
        @Bean
        TransactionTypeStore store() {
            return InMemoryTransactionTypeStore.seeded();
        }
    }

    @Autowired
    MockMvc mvc;

    @Test
    @DisplayName("BR-TT-010 GET list returns a 7-row page")
    void list() throws Exception {
        mvc.perform(get("/api/v1/transaction-types")).andExpect(status().isOk())
                .andExpect(jsonPath("$.items.length()").value(7))
                .andExpect(jsonPath("$.items[0].code").value("01"));
    }

    @Test
    @DisplayName("BR-TT-002 validation failure maps to 400 with rule ID")
    void validation() throws Exception {
        mvc.perform(post("/api/v1/transaction-types").contentType(MediaType.APPLICATION_JSON)
                        .content("{\"code\":\"AB\",\"description\":\"X\"}"))
                .andExpect(status().isBadRequest()).andExpect(jsonPath("$.ruleId").value("BR-TT-002"));
    }

    @Test
    @DisplayName("BR-TT-006 invalid description characters -> 400")
    void descriptionChars() throws Exception {
        mvc.perform(post("/api/v1/transaction-types").contentType(MediaType.APPLICATION_JSON)
                        .content("{\"code\":\"08\",\"description\":\"CASH-BACK!\"}"))
                .andExpect(status().isBadRequest()).andExpect(jsonPath("$.ruleId").value("BR-TT-006"));
    }

    @Test
    @DisplayName("BR-TT-030 create -> 201, duplicate -> 409")
    void create() throws Exception {
        mvc.perform(post("/api/v1/transaction-types").contentType(MediaType.APPLICATION_JSON)
                .content("{\"code\":\"8\",\"description\":\"CASHBACK\"}")).andExpect(status().isCreated())
                .andExpect(jsonPath("$.item.code").value("08"));
        mvc.perform(post("/api/v1/transaction-types").contentType(MediaType.APPLICATION_JSON)
                .content("{\"code\":\"02\",\"description\":\"DUP\"}")).andExpect(status().isConflict())
                .andExpect(jsonPath("$.sqlCode").value(-803));
    }

    @Test
    @DisplayName("BR-TT-008 no-change update -> 422; BR-TT-020 unknown -> 404")
    void update() throws Exception {
        mvc.perform(put("/api/v1/transaction-types/03").contentType(MediaType.APPLICATION_JSON)
                .content("{\"description\":\"credit\"}")).andExpect(status().isUnprocessableEntity());
        mvc.perform(put("/api/v1/transaction-types/97").contentType(MediaType.APPLICATION_JSON)
                .content("{\"description\":\"X\"}")).andExpect(status().isNotFound())
                .andExpect(jsonPath("$.ruleId").value("BR-TT-020"));
    }

    @Test
    @DisplayName("BR-TT-050 delete without confirm -> 428; BR-TT-051 child rows -> 409")
    void deleteFlow() throws Exception {
        mvc.perform(delete("/api/v1/transaction-types/04")).andExpect(status().isPreconditionRequired());
        mvc.perform(delete("/api/v1/transaction-types/01").param("confirm", "true"))
                .andExpect(status().isConflict()).andExpect(jsonPath("$.sqlCode").value(-532));
    }

    @Test
    @DisplayName("BR-TT-071 batch endpoint accepts text/plain records")
    void batch() throws Exception {
        mvc.perform(post("/api/v1/transaction-types/batch").contentType(MediaType.TEXT_PLAIN)
                        .content("A09NINE\n*comment\nX00BAD\n"))
                .andExpect(status().isOk()).andExpect(jsonPath("$.returnCode").value(4))
                .andExpect(jsonPath("$.inserted").value(1));
    }

    @Test
    @DisplayName("BR-TT-018 one action per request: only PUT (U) and DELETE (D) act on a row")
    void br_tt_018_singleActionPerRequest() throws Exception {
        mvc.perform(patch("/api/v1/transaction-types/01")).andExpect(status().isMethodNotAllowed());
    }

    @Test
    @DisplayName("BR-TT-080 an empty extract is an empty file, not a blank record")
    void br_tt_080_emptyExtract() {
        assertThat(TransactionTypeController.toRecords(List.of())).isEmpty();
        assertThat(TransactionTypeController.toRecords(List.of("a", "b"))).isEqualTo("a\nb\n");
    }
}
