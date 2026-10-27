package com.carddemo.transaction.web;

import com.carddemo.transaction.batch.posting.TransactionPostingJobConfig;
import com.carddemo.transaction.support.TestData;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.batch.core.Job;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.startsWith;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * COTRN00C / COTRN01C / COTRN02C over the TRANSACT data produced by TransactionPostingJob.
 */
@SpringBootTest
@AutoConfigureMockMvc
@Import(TestData.class)
class TransactionControllerTest {

    @Autowired
    MockMvc mvc;
    @Autowired
    TestData.Fixtures data;
    @Autowired
    @Qualifier(TransactionPostingJobConfig.JOB_NAME)
    Job postingJob;

    @BeforeEach
    void postSampleDay() throws Exception {
        data.reseed();
        data.run(postingJob, TestData.SAMPLE_DALYTRAN);
    }

    private static String body(String acct, String card, String amount, String origDate, String confirm) {
        return """
                {"accountId":%s,"cardNumber":%s,"typeCd":"01","categoryCd":"0001","source":"POS TERM",
                 "description":"Online add","amount":%s,"origDate":%s,"procDate":"2024-01-16",
                 "merchantId":"800000000","merchantName":"Test Merchant","merchantCity":"Test City",
                 "merchantZip":"12345","confirm":%s}
                """.formatted(q(acct), q(card), q(amount), q(origDate), q(confirm));
    }

    private static String q(String s) {
        return s == null ? "null" : "\"" + s + "\"";
    }

    @Test
    void listShowsTenRowsPerPageInTranIdOrderWithEditedAmountAndMmDdYyDate() throws Exception {
        mvc.perform(get("/api/transactions"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.transactions", hasSize(10)))
                .andExpect(jsonPath("$.nextPage").value(true))
                .andExpect(jsonPath("$.transactions[0].date").value(startsWith("")))
                .andExpect(jsonPath("$.transactions[0].amount").value(org.hamcrest.Matchers.matchesPattern("[+-]\\d{8}\\.\\d{2}")));
    }

    @Test
    void pf8PagesForwardAndPf7PagesBackToTheSameRows() throws Exception {
        String page1 = mvc.perform(get("/api/transactions")).andReturn().getResponse().getContentAsString();
        String first = com.jayway.jsonpath.JsonPath.read(page1, "$.firstTranId");
        String last = com.jayway.jsonpath.JsonPath.read(page1, "$.lastTranId");
        String page2 = mvc.perform(get("/api/transactions").param("after", last))
                .andExpect(jsonPath("$.transactions", hasSize(10))).andReturn().getResponse().getContentAsString();
        String p2first = com.jayway.jsonpath.JsonPath.read(page2, "$.firstTranId");
        mvc.perform(get("/api/transactions").param("before", p2first))
                .andExpect(jsonPath("$.firstTranId").value(first))
                .andExpect(jsonPath("$.lastTranId").value(last));
        mvc.perform(get("/api/transactions").param("before", first))
                .andExpect(jsonPath("$.message").value("You are already at the top of the page..."));
    }

    @Test
    void listRejectsNonNumericStartingTranId() throws Exception {
        mvc.perform(get("/api/transactions").param("tranId", "ABC"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Tran ID must be Numeric ..."));
    }

    @Test
    void viewReturnsAPostedTransactionAndNotFoundForAnUnknownId() throws Exception {
        mvc.perform(get("/api/transactions/0000000000683580"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.amount").value(504.77))
                .andExpect(jsonPath("$.procTs").value("2026-10-27-10.15.30.120000"));
        mvc.perform(get("/api/transactions/0000000000000000"))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("Transaction ID NOT found..."));
    }

    @Test
    void addAssignsLastTranIdPlusOneAndPersistsToTheSharedTransactTable() throws Exception {
        String lastId = data.transactions().findTopByOrderByTranIdDesc().orElseThrow().getTranId();
        String expected = String.format("%016d", Long.parseLong(lastId) + 1);
        mvc.perform(post("/api/transactions").contentType(MediaType.APPLICATION_JSON)
                        .content(body("1", null, "+00000012.34", "2024-01-15", "Y")))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.tranId").value(expected))
                .andExpect(jsonPath("$.accountId").value("00000000001"))
                .andExpect(jsonPath("$.message").value("Transaction added successfully.  Your Tran ID is " + expected + "."));
        mvc.perform(get("/api/transactions/" + expected))
                .andExpect(jsonPath("$.amount").value(12.34))
                .andExpect(jsonPath("$.origTs").value("2024-01-15"));
    }

    @Test
    void addResolvesAccountFromCardNumberThroughXref() throws Exception {
        mvc.perform(post("/api/transactions").contentType(MediaType.APPLICATION_JSON)
                        .content(body(null, "0500024453765740", "-00000001.00", "2024-01-15", "y")))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.accountId").value("00000000050"));
    }

    @Test
    void addRequiresConfirmationAndRejectsInvalidConfirmValues() throws Exception {
        mvc.perform(post("/api/transactions").contentType(MediaType.APPLICATION_JSON)
                        .content(body("1", null, "+00000012.34", "2024-01-15", null)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Confirm to add this transaction..."));
        mvc.perform(post("/api/transactions").contentType(MediaType.APPLICATION_JSON)
                        .content(body("1", null, "+00000012.34", "2024-01-15", "X")))
                .andExpect(jsonPath("$.message").value("Invalid value. Valid values are (Y/N)..."));
    }

    @Test
    void addValidationMessagesMatchCotrn02c() throws Exception {
        String[][] cases = {
                {null, null, "+00000012.34", "2024-01-15", "Account or Card Number must be entered..."},
                {"12A", null, "+00000012.34", "2024-01-15", "Account ID must be Numeric..."},
                {"99999999", null, "+00000012.34", "2024-01-15", "Account ID NOT found..."},
                {null, "9999999999999999", "+00000012.34", "2024-01-15", "Card Number NOT found..."},
                {"1", null, "12.34", "2024-01-15", "Amount should be in format -99999999.99"},
                {"1", null, "+00000012.34", "15/01/2024", "Orig Date should be in format YYYY-MM-DD"},
                {"1", null, "+00000012.34", "2024-02-30", "Orig Date - Not a valid date..."},
        };
        for (String[] c : cases) {
            mvc.perform(post("/api/transactions").contentType(MediaType.APPLICATION_JSON)
                            .content(body(c[0], c[1], c[2], c[3], "Y")))
                    .andExpect(jsonPath("$.message").value(c[4]));
        }
    }
}
