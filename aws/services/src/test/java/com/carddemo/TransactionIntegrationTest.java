package com.carddemo;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import org.junit.jupiter.api.Test;

class TransactionIntegrationTest extends IntegrationTestBase {

    private static Map<String, Object> newTransaction() {
        Map<String, Object> body = new HashMap<>();
        body.put("cardNum", "0500024453765740");
        body.put("typeCd", "01");
        body.put("catCd", "0001");
        body.put("source", "POS TERM");
        body.put("description", "Purchase at test shop");
        body.put("amt", "-00000123.45");
        body.put("origDate", "2026-09-01");
        body.put("procDate", "2026-09-02");
        body.put("merchantId", "000012345");
        body.put("merchantName", "Test Shop");
        body.put("merchantCity", "Springfield");
        body.put("merchantZip", "12345");
        return body;
    }

    @Test
    void listPagesTenTransactionsLikeCotrn00c() throws Exception {
        String token = userToken();
        JsonNode first = body(mvc.perform(as(token, get("/api/v1/transactions")))
                .andExpect(status().isOk()).andReturn());
        assertThat(first.get("items")).hasSize(10);
        assertThat(first.get("firstKey").asText()).isEqualTo("0000000000683580");
        assertThat(first.get("hasNext").asBoolean()).isTrue();
        JsonNode next = body(mvc.perform(as(token, get("/api/v1/transactions")
                .param("startKey", first.get("lastKey").asText()))).andExpect(status().isOk()).andReturn());
        assertThat(next.get("items")).hasSize(10);
        assertThat(next.get("hasPrev").asBoolean()).isTrue();
    }

    @Test
    void listRejectsNonNumericStartKey() throws Exception {
        mvc.perform(as(userToken(), get("/api/v1/transactions").param("startKey", "ABC")))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Tran ID must be Numeric ..."));
    }

    @Test
    void detailReturnsTransaction() throws Exception {
        mvc.perform(as(userToken(), get("/api/v1/transactions/0000000000683580")))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.typeCd").value("01"))
                .andExpect(jsonPath("$.catCd").value(1))
                .andExpect(jsonPath("$.source").value("POS TERM"))
                .andExpect(jsonPath("$.merchantName").value("Abshire-Lowe"));
    }

    @Test
    void detailReportsMissingTransaction() throws Exception {
        mvc.perform(as(userToken(), get("/api/v1/transactions/9999999999999999")))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("Transaction ID NOT found..."));
    }

    @Test
    void createAllocatesNextTranId() throws Exception {
        String token = userToken();
        mvc.perform(as(token, withJson(post("/api/v1/transactions"), newTransaction())))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.tranId").value("0000000996722788"))
                .andExpect(jsonPath("$.message")
                        .value("Transaction added successfully. Your Tran ID is 0000000996722788."));
        mvc.perform(as(token, get("/api/v1/transactions/0000000996722788")))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.amt").value("-123.45"))
                .andExpect(jsonPath("$.cardNum").value("0500024453765740"));
    }

    @Test
    void createResolvesCardFromAccount() throws Exception {
        Map<String, Object> body = newTransaction();
        body.remove("cardNum");
        body.put("acctId", "00000000050");
        String tranId = body(mvc.perform(as(userToken(), withJson(post("/api/v1/transactions"), body)))
                .andExpect(status().isCreated()).andReturn()).get("tranId").asText();
        String card = jdbc.sql("SELECT card_num FROM transaction WHERE tran_id = :id").param("id", tranId)
                .query(String.class).single();
        assertThat(card).isEqualTo("0500024453765740");
    }

    @Test
    void createValidatesInput() throws Exception {
        String token = userToken();
        Map<String, Object> missingKeys = newTransaction();
        missingKeys.remove("cardNum");
        mvc.perform(as(token, withJson(post("/api/v1/transactions"), missingKeys)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Account or Card Number must be entered..."));

        Map<String, Object> bothKeys = newTransaction();
        bothKeys.put("acctId", "00000000050");
        mvc.perform(as(token, withJson(post("/api/v1/transactions"), bothKeys)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Enter either Account ID or Card Number, not both"));

        Map<String, Object> badAmount = newTransaction();
        badAmount.put("amt", "12.3");
        badAmount.put("origDate", "2026-02-30");
        JsonNode error = body(mvc.perform(as(token, withJson(post("/api/v1/transactions"), badAmount)))
                .andExpect(status().isBadRequest()).andReturn());
        assertThat(error.get("legacyProgram").asText()).isEqualTo("COTRN02C");
        assertThat(error.get("fieldErrors").findValuesAsText("message"))
                .contains("Amount should be in format -99999999.99");

        Map<String, Object> unknownCard = newTransaction();
        unknownCard.put("cardNum", "1111111111111111");
        mvc.perform(as(token, withJson(post("/api/v1/transactions"), unknownCard)))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("Card Number NOT found..."));
    }

    @Test
    void concurrentCreatesGetDistinctSequentialIds() throws Exception {
        String token = userToken();
        ExecutorService pool = Executors.newFixedThreadPool(4);
        try {
            List<Callable<String>> calls = new ArrayList<>();
            for (int i = 0; i < 8; i++) {
                calls.add(() -> body(mvc.perform(as(token, withJson(post("/api/v1/transactions"), newTransaction())))
                        .andExpect(status().isCreated()).andReturn()).get("tranId").asText());
            }
            Set<String> ids = new HashSet<>();
            for (Future<String> f : pool.invokeAll(calls)) {
                ids.add(f.get());
            }
            assertThat(ids).hasSize(8);
            assertThat(ids).contains("0000000996722788", "0000000996722795");
        } finally {
            pool.shutdown();
        }
    }
}
