package com.carddemo.transaction.api;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.transaction.repository.TransactionRepository;
import com.carddemo.transaction.support.StubGateways;
import java.math.BigDecimal;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

@SpringBootTest
@AutoConfigureMockMvc
@Import(StubGateways.class)
class TransactionControllerTest {

    private static final String CARD = "4111111111111111";
    private static final long ACCOUNT = 11111111111L;

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private TransactionRepository transactions;

    @Autowired
    private StubGateways.StubCardGateway cards;

    @Autowired
    private StubGateways.StubAccountGateway accounts;

    @BeforeEach
    void setUp() {
        transactions.deleteAll();
        cards.clear();
        accounts.clear();
        cards.register(CARD, 100000001L, ACCOUNT);
        accounts.register(ACCOUNT, "DEFAULT", new BigDecimal("250.00"));
    }

    @Test
    void addsATransactionForAKnownCard() throws Exception {
        mockMvc.perform(post("/api/v1/transactions")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("""
                                {
                                  "cardNumber": "4111111111111111",
                                  "typeCode": "01",
                                  "categoryCode": 1,
                                  "source": "POS TERM",
                                  "description": "GROCERIES",
                                  "amount": 42.50
                                }
                                """))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.amount").value(42.50))
                .andExpect(jsonPath("$.transactionId").isNotEmpty());

        assertThat(transactions.count()).isEqualTo(1);
    }

    @Test
    void rejectsATransactionForAnUnknownCard() throws Exception {
        mockMvc.perform(post("/api/v1/transactions")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("""
                                {
                                  "cardNumber": "4999999999999999",
                                  "typeCode": "01",
                                  "categoryCode": 1,
                                  "amount": 42.50
                                }
                                """))
                .andExpect(status().isNotFound());
    }

    @Test
    void rejectsAMalformedCardNumber() throws Exception {
        mockMvc.perform(post("/api/v1/transactions")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("""
                                {"cardNumber": "41111", "typeCode": "01", "categoryCode": 1, "amount": 1.00}
                                """))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].field").value("cardNumber"));
    }

    @Test
    void listsTransactionsFilteredByCard() throws Exception {
        mockMvc.perform(post("/api/v1/transactions")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("""
                                {"cardNumber": "4111111111111111", "typeCode": "01",
                                 "categoryCode": 1, "amount": 10.00}
                                """))
                .andExpect(status().isCreated());

        mockMvc.perform(get("/api/v1/transactions").param("cardNumber", CARD))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.totalElements").value(1));
    }

    @Test
    void paysTheWholeBalanceAsABillPayment() throws Exception {
        mockMvc.perform(post("/api/v1/bill-payments")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("{\"accountId\": 11111111111}"))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.amount").value(250.00))
                .andExpect(jsonPath("$.typeCode").value("02"))
                .andExpect(jsonPath("$.description").value("BILL PAYMENT - ONLINE"));

        assertThat(accounts.postings()).containsExactly(new BigDecimal("-250.00"));
    }

    @Test
    void returns404ForAnUnknownTransaction() throws Exception {
        mockMvc.perform(get("/api/v1/transactions/{id}", "0000000000000000"))
                .andExpect(status().isNotFound());
    }
}
