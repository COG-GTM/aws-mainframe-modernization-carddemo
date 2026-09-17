package com.carddemo.account.api;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.account.domain.Account;
import com.carddemo.account.repository.AccountRepository;
import java.math.BigDecimal;
import java.time.LocalDate;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

@SpringBootTest
@AutoConfigureMockMvc
class AccountControllerTest {

    private static final long ACCT_ID = 12345678901L;

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private AccountRepository accounts;

    @BeforeEach
    void setUp() {
        accounts.deleteAll();
        Account account = new Account(ACCT_ID);
        account.setCurrBal(new BigDecimal("42.00"));
        account.setCreditLimit(new BigDecimal("1000.00"));
        account.setCashCreditLimit(new BigDecimal("200.00"));
        account.setOpenDate(LocalDate.of(2021, 2, 3));
        account.setExpirationDate(LocalDate.of(2031, 2, 3));
        account.setGroupId("DEFAULT");
        accounts.save(account);
    }

    @Test
    void returnsTheAccountViewScreenFields() throws Exception {
        mockMvc.perform(get("/api/v1/accounts/{id}", ACCT_ID))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.accountId").value(ACCT_ID))
                .andExpect(jsonPath("$.currentBalance").value(42.00))
                .andExpect(jsonPath("$.groupId").value("DEFAULT"));
    }

    @Test
    void returns404ForAnUnknownAccount() throws Exception {
        mockMvc.perform(get("/api/v1/accounts/{id}", 99999999999L))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.status").value(404));
    }

    @Test
    void updatesTheAccount() throws Exception {
        mockMvc.perform(put("/api/v1/accounts/{id}", ACCT_ID)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("""
                                {
                                  "activeStatus": "N",
                                  "creditLimit": 2000.00,
                                  "cashCreditLimit": 300.00,
                                  "openDate": "2021-02-03",
                                  "expirationDate": "2032-02-03",
                                  "reissueDate": "2027-02-03",
                                  "addressZip": "60601",
                                  "groupId": "ZEROAPR"
                                }
                                """))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.activeStatus").value("N"))
                .andExpect(jsonPath("$.creditLimit").value(2000.00));
    }

    @Test
    void rejectsACashLimitAboveTheCreditLimit() throws Exception {
        mockMvc.perform(put("/api/v1/accounts/{id}", ACCT_ID)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("""
                                {
                                  "activeStatus": "Y",
                                  "creditLimit": 1000.00,
                                  "cashCreditLimit": 1500.00,
                                  "openDate": "2021-02-03",
                                  "expirationDate": "2032-02-03"
                                }
                                """))
                .andExpect(status().isUnprocessableEntity())
                .andExpect(jsonPath("$.status").value(422));
    }

    @Test
    void rejectsAnInvalidActiveStatusWithFieldErrors() throws Exception {
        mockMvc.perform(put("/api/v1/accounts/{id}", ACCT_ID)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("""
                                {
                                  "activeStatus": "X",
                                  "creditLimit": 1000.00,
                                  "cashCreditLimit": 100.00,
                                  "openDate": "2021-02-03",
                                  "expirationDate": "2032-02-03"
                                }
                                """))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].field").value("activeStatus"));
    }

    @Test
    void postsAnAmountAndReturns422WithTheCobolReasonCodeWhenRejected() throws Exception {
        mockMvc.perform(org.springframework.test.web.servlet.request.MockMvcRequestBuilders
                        .post("/api/v1/accounts/{id}/postings", ACCT_ID)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("{\"amount\": 25.00, \"transactionDate\": \"2024-05-01\"}"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.posted").value(true))
                .andExpect(jsonPath("$.currentBalance").value(67.00));

        mockMvc.perform(org.springframework.test.web.servlet.request.MockMvcRequestBuilders
                        .post("/api/v1/accounts/{id}/postings", ACCT_ID)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("{\"amount\": 5000.00, \"transactionDate\": \"2024-05-01\"}"))
                .andExpect(status().isUnprocessableEntity())
                .andExpect(jsonPath("$.reasonCode").value(102));
    }
}
