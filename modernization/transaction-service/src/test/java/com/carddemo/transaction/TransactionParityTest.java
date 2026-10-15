package com.carddemo.transaction;

import static org.hamcrest.Matchers.hasSize;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.common.testing.ParityTestSupport;
import java.math.BigDecimal;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;

/** Parity with COTRN00C, COTRN01C, COTRN02C, COBIL00C and CORPT00C. */
@SpringBootTest
@AutoConfigureMockMvc
class TransactionParityTest extends ParityTestSupport {

    @Autowired
    MockMvc mvc;

    @Autowired
    JdbcTemplate jdbc;

    private ResultActions postJson(String path, String body) throws Exception {
        return mvc.perform(post(path).header("Authorization", userToken())
                .contentType(MediaType.APPLICATION_JSON).content(body));
    }

    @Test
    void cotrn00cTenRowsPerPageWithPf7Pf8() throws Exception {
        mvc.perform(get("/api/v1/transactions").header("Authorization", userToken()))
                .andExpect(jsonPath("$.items", hasSize(10)))
                .andExpect(jsonPath("$.firstKey").value("0000000000000001"))
                .andExpect(jsonPath("$.lastKey").value("0000000000000010"))
                .andExpect(jsonPath("$.hasNextPage").value(true));
        mvc.perform(get("/api/v1/transactions?after=0000000000000010&page=1").header("Authorization", userToken()))
                .andExpect(jsonPath("$.items", hasSize(2)))
                .andExpect(jsonPath("$.pageNumber").value(2));
        mvc.perform(get("/api/v1/transactions?before=0000000000000011&page=2").header("Authorization", userToken()))
                .andExpect(jsonPath("$.items", hasSize(10)))
                .andExpect(jsonPath("$.items[0].tranId").value("0000000000000001"))
                .andExpect(jsonPath("$.pageNumber").value(1));
        mvc.perform(get("/api/v1/transactions?startKey=5").header("Authorization", userToken()))
                .andExpect(jsonPath("$.items[0].tranId").value("0000000000000005"));
        mvc.perform(get("/api/v1/transactions?startKey=ABC").header("Authorization", userToken()))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Tran ID must be Numeric ..."));
    }

    @Test
    void cotrn01cViewAndNotFound() throws Exception {
        mvc.perform(get("/api/v1/transactions/0000000000000003").header("Authorization", userToken()))
                .andExpect(jsonPath("$.amount").value(30.03))
                .andExpect(jsonPath("$.merchantName").value("Store 03"));
        mvc.perform(get("/api/v1/transactions/9999999999999999").header("Authorization", userToken()))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("Transaction ID NOT found..."));
    }

    private static final String ADD = "{\"cardNum\":\"4111111111111111\",\"typeCd\":\"01\",\"catCd\":\"1\","
            + "\"source\":\"POS TERM\",\"description\":\"Coffee\",\"amount\":\"+00000004.50\","
            + "\"origDate\":\"2024-01-15\",\"procDate\":\"2024-01-15\",\"merchantId\":\"123456789\","
            + "\"merchantName\":\"Cafe\",\"merchantCity\":\"Zurich\",\"merchantZip\":\"8001\"%s}";

    @Test
    void cotrn02cRequiresConfirmationThenAddsWithNextId() throws Exception {
        postJson("/api/v1/transactions", String.format(ADD, ""))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.confirmationRequired").value(true))
                .andExpect(jsonPath("$.message").value("Confirm to add this transaction..."));
        postJson("/api/v1/transactions", String.format(ADD, ",\"confirm\":\"Y\""))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.tranId").value("0000000000000013"))
                .andExpect(jsonPath("$.message")
                        .value("Transaction added successfully.  Your Tran ID is 0000000000000013."));
    }

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {
            "\"cardNum\":\"4111111111111111\"|\"cardNum\":\"9999999999999999\"|404|Card Number NOT found...",
            "\"amount\":\"+00000004.50\"|\"amount\":\"4.5\"|400|Amount should be in format -99999999.99",
            "\"origDate\":\"2024-01-15\"|\"origDate\":\"2024-02-30\"|400|Orig Date - Not a valid date...",
            "\"typeCd\":\"01\"|\"typeCd\":\"AB\"|400|Type CD must be Numeric...",
            "\"description\":\"Coffee\"|\"description\":\"\"|400|Description can NOT be empty..."
    })
    void cotrn02cFieldEdits(String from, String to, int status, String message) throws Exception {
        postJson("/api/v1/transactions", String.format(ADD, ",\"confirm\":\"Y\"").replace(from, to))
                .andExpect(status().is(status))
                .andExpect(jsonPath("$.message").value(message));
    }

    @Test
    void cobil00cPaysFullBalanceAfterConfirmation() throws Exception {
        postJson("/api/v1/bill-payments", "{\"acctId\":\"00000000001\"}")
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.confirmationRequired").value(true))
                .andExpect(jsonPath("$.currentBalance").value(194.00));
        postJson("/api/v1/bill-payments", "{\"acctId\":\"00000000001\",\"confirm\":\"Y\"}")
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.message")
                        .value("Payment successful.  Your Transaction ID is 0000000000000013."))
                .andExpect(jsonPath("$.currentBalance").value(0));
        BigDecimal bal = jdbc.queryForObject("select curr_bal from account where acct_id = 1", BigDecimal.class);
        org.assertj.core.api.Assertions.assertThat(bal).isEqualByComparingTo("0");
        mvc.perform(get("/api/v1/transactions/0000000000000013").header("Authorization", userToken()))
                .andExpect(jsonPath("$.typeCd").value("02"))
                .andExpect(jsonPath("$.catCd").value(2))
                .andExpect(jsonPath("$.amount").value(194.00))
                .andExpect(jsonPath("$.description").value("BILL PAYMENT - ONLINE"))
                .andExpect(jsonPath("$.cardNum").value("4111111111111111"));
        postJson("/api/v1/bill-payments", "{\"acctId\":\"00000000001\",\"confirm\":\"Y\"}")
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("You have nothing to pay..."));
    }

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {
            "{\"acctId\":\"\"}|400|Acct ID can NOT be empty...",
            "{\"acctId\":\"99999999999\"}|404|Account ID NOT found...",
            "{\"acctId\":\"00000000001\",\"confirm\":\"X\"}|400|Invalid value. Valid values are (Y/N)..."
    })
    void cobil00cErrors(String body, int status, String message) throws Exception {
        postJson("/api/v1/bill-payments", body).andExpect(status().is(status))
                .andExpect(jsonPath("$.message").value(message));
    }

    @Test
    void corpt00cQueuesCustomReport() throws Exception {
        postJson("/api/v1/reports/transactions",
                "{\"reportType\":\"custom\",\"startDate\":\"2022-06-01\",\"endDate\":\"2022-06-30\",\"confirm\":\"Y\"}")
                .andExpect(status().isAccepted())
                .andExpect(jsonPath("$.status").value("SUBMITTED"))
                .andExpect(jsonPath("$.requestedBy").value("USER0001"));
        postJson("/api/v1/reports/transactions", "{\"reportType\":\"monthly\"}")
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Please confirm to print the Monthly report..."));
        postJson("/api/v1/reports/transactions", "{}")
                .andExpect(jsonPath("$.message").value("Select a report type to print report..."));
    }

    @Test
    void referenceData() throws Exception {
        mvc.perform(get("/api/v1/transaction-types").header("Authorization", userToken()))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$[0].typeCd").value("01"));
    }
}
