package com.carddemo;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import org.junit.jupiter.api.Test;

class BillPaymentIntegrationTest extends IntegrationTestBase {

    @Test
    void balanceShowsCurrentBalance() throws Exception {
        mvc.perform(as(userToken(), get("/api/v1/bill-payments/00000000001")))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.acctId").value(1))
                .andExpect(jsonPath("$.currBal").value("194.00"));
    }

    @Test
    void paymentPaysFullBalanceAndRecordsTransaction() throws Exception {
        String token = userToken();
        String tranId = body(mvc.perform(as(token, withJson(post("/api/v1/bill-payments"),
                Map.of("acctId", "00000000001"))))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.amount").value("194.00"))
                .andExpect(jsonPath("$.tranId").value("0000000996722788"))
                .andExpect(jsonPath("$.message")
                        .value("Payment successful. Your Transaction ID is 0000000996722788."))
                .andReturn()).get("tranId").asText();

        BigDecimal balance = jdbc.sql("SELECT curr_bal FROM account WHERE acct_id = 1").query(BigDecimal.class)
                .single();
        assertThat(balance).isEqualByComparingTo("0");
        Map<String, Object> tran = jdbc.sql("SELECT * FROM transaction WHERE tran_id = :id").param("id", tranId)
                .query().singleRow();
        assertThat(tran.get("type_cd")).isEqualTo("02");
        assertThat(((Number) tran.get("cat_cd")).intValue()).isEqualTo(2);
        assertThat(tran.get("description")).isEqualTo("BILL PAYMENT - ONLINE");
        assertThat(((Number) tran.get("merchant_id")).intValue()).isEqualTo(999999999);
        assertThat((BigDecimal) tran.get("amt")).isEqualByComparingTo("194.00");

        mvc.perform(as(token, withJson(post("/api/v1/bill-payments"), Map.of("acctId", "00000000001"))))
                .andExpect(status().isUnprocessableEntity())
                .andExpect(jsonPath("$.errorCode").value("BUSINESS_RULE"))
                .andExpect(jsonPath("$.message").value("You have nothing to pay..."));
    }

    @Test
    void paymentValidatesAccount() throws Exception {
        String token = userToken();
        mvc.perform(as(token, withJson(post("/api/v1/bill-payments"), Map.of("acctId", ""))))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Acct ID can NOT be empty..."))
                .andExpect(jsonPath("$.legacyProgram").value("COBIL00C"));
        mvc.perform(as(token, withJson(post("/api/v1/bill-payments"), Map.of("acctId", "99999999999"))))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("Account ID NOT found..."));
    }

    @Test
    void concurrentPaymentsSerializeOnAccountLock() throws Exception {
        String token = userToken();
        ExecutorService pool = Executors.newFixedThreadPool(4);
        try {
            List<Callable<Integer>> calls = new ArrayList<>();
            for (int i = 0; i < 4; i++) {
                calls.add(() -> mvc.perform(as(token, withJson(post("/api/v1/bill-payments"),
                        Map.of("acctId", "00000000001")))).andReturn().getResponse().getStatus());
            }
            List<Integer> statuses = new ArrayList<>();
            for (Future<Integer> f : pool.invokeAll(calls)) {
                statuses.add(f.get());
            }
            assertThat(statuses).containsExactlyInAnyOrder(201, 422, 422, 422);
        } finally {
            pool.shutdown();
        }
        long payments = jdbc.sql("SELECT COUNT(*) FROM transaction WHERE description = 'BILL PAYMENT - ONLINE'")
                .query(Long.class).single();
        assertThat(payments).isEqualTo(1);
    }
}
