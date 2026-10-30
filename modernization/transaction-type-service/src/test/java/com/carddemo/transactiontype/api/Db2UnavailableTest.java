package com.carddemo.transactiontype.api;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.transactiontype.domain.InMemoryTransactionTypeStore;
import com.carddemo.transactiontype.domain.ListFilter;
import com.carddemo.transactiontype.domain.TransactionTypeService;
import com.carddemo.transactiontype.domain.TransactionTypeStore;
import com.carddemo.transactiontype.domain.batch.BatchMaintenanceService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Import;
import org.springframework.dao.DataAccessResourceFailureException;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.web.servlet.MockMvc;
import com.carddemo.transactiontype.config.SecurityConfig;

@WebMvcTest(TransactionTypeController.class)
@Import({SecurityConfig.class, TransactionTypeService.class, BatchMaintenanceService.class, Db2UnavailableTest.DownStore.class})
@WithMockUser(roles = SecurityConfig.MAINTAINER)
class Db2UnavailableTest {

    @TestConfiguration
    static class DownStore {
        @Bean
        TransactionTypeStore store() {
            return new InMemoryTransactionTypeStore() {
                @Override
                public long count(ListFilter filter) {
                    throw new DataAccessResourceFailureException("connection refused");
                }
            };
        }
    }

    @Autowired
    MockMvc mvc;

    @Test
    @DisplayName("BR-TT-061 Db2 unreachable -> 503 instead of serving data")
    void br_tt_061_db2Down() throws Exception {
        mvc.perform(get("/api/v1/transaction-types"))
                .andExpect(status().isServiceUnavailable())
                .andExpect(jsonPath("$.ruleId").value("BR-TT-061"));
    }
}
