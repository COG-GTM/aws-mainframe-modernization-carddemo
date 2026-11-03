package com.carddemo.transaction.flow;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.testsupport.flow.CobolFlow;
import com.carddemo.testsupport.flow.ScreenKey;
import com.carddemo.testsupport.flow.ServiceStandIn;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.ResultActions;

/**
 * CT00 transaction list (COTRN00C: 10 rows, TRNIDIN start key, PF7/PF8), CT01 view (COTRN01C), CT02 add
 * (COTRN02C) and CB00 bill payment (COBIL00C) on the sample data. Expected rows come from the sample
 * DALYTRAN copied into TRANSACT ({@link TransactionFlowTestSupport}).
 */
class TransactionFlowTest extends TransactionFlowTestSupport {

    private List<String> sampleIds() {
        return jdbc.queryForList("SELECT tran_id FROM transaction.daily_transactions ORDER BY tran_id", String.class);
    }

    private ResultActions list(String... params) throws Exception {
        var request = get("/api/v1/transactions");
        for (int i = 0; i < params.length; i += 2) {
            request.param(params[i], params[i + 1]);
        }
        return mockMvc.perform(asUser(request));
    }

    private ResultActions add(Map<String, String> body) throws Exception {
        return mockMvc.perform(asUser(post("/api/v1/transactions").contentType(MediaType.APPLICATION_JSON)
                .content(ServiceStandIn.JSON.writeValueAsString(body))));
    }

    private ResultActions pay(String json) throws Exception {
        return mockMvc.perform(asUser(post("/api/v1/bill-payments").contentType(MediaType.APPLICATION_JSON).content(json)));
    }

    /** A COTRN2A screen that passes every edit, for card 9680294154603697 / account 1. */
    private static Map<String, String> addScreen() {
        Map<String, String> b = new LinkedHashMap<>();
        b.put("accountId", "00000000001");
        b.put("cardNumber", "");
        b.put("typeCode", "01");
        b.put("categoryCode", "0001");
        b.put("source", "POS TERM");
        b.put("description", "Flow test purchase");
        b.put("amount", "-00000012.34");
        b.put("originDate", "2022-07-18");
        b.put("processDate", "2022-07-18");
        b.put("merchantId", "800000001");
        b.put("merchantName", "Flow Test Merchant");
        b.put("merchantCity", "Austin");
        b.put("merchantZip", "78701");
        return b;
    }

    // ---- COTRN00C -------------------------------------------------------------------------------

    @Test
    @CobolFlow(program = "COTRN00C", paragraph = "PROCESS-PAGE-FORWARD", key = ScreenKey.ENTER)
    void COTRN00C_PROCESS_PAGE_FORWARD_firstScreenListsTenTransactionsInIdOrder() throws Exception {
        List<String> ids = sampleIds();
        list("size", "10")
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content.length()").value(10))
                .andExpect(jsonPath("$.content[0].transactionId").value(ids.get(0)))
                .andExpect(jsonPath("$.content[9].transactionId").value(ids.get(9)));
    }

    @Test
    @CobolFlow(program = "COTRN00C", paragraph = "STARTBR-TRANSACT-FILE", key = ScreenKey.ENTER)
    void COTRN00C_STARTBR_TRANSACT_FILE_typedTranIdStartsTheBrowse() throws Exception {
        List<String> ids = sampleIds();
        list("size", "10", "startTransactionId", ids.get(42))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].transactionId").value(ids.get(42)))
                .andExpect(jsonPath("$.content[1].transactionId").value(ids.get(43)));
        list("startTransactionId", "12AB")
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].message").value("Tran ID must be Numeric ..."));
    }

    @Test
    @CobolFlow(program = "COTRN00C", paragraph = "PROCESS-PF8-KEY", key = ScreenKey.PF8)
    void COTRN00C_PROCESS_PF8_KEY_nextScreenStartsAfterTheLastRowShown() throws Exception {
        List<String> ids = sampleIds();
        list("size", "10", "page", "1")
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].transactionId").value(ids.get(10)))
                .andExpect(jsonPath("$.content[9].transactionId").value(ids.get(19)));
    }

    @Test
    @CobolFlow(program = "COTRN00C", paragraph = "PROCESS-PF7-KEY", key = ScreenKey.PF7)
    void COTRN00C_PROCESS_PF7_KEY_previousScreenAndNothingBeforeTheTop() throws Exception {
        List<String> ids = sampleIds();
        list("size", "10", "page", "0").andExpect(jsonPath("$.content[0].transactionId").value(ids.get(0)));
        list("size", "10", "page", "-1")
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].field").value("page"));
    }

    // ---- COTRN01C -------------------------------------------------------------------------------

    @Test
    @CobolFlow(program = "COTRN01C", paragraph = "READ-TRANSACT-FILE", key = ScreenKey.ENTER)
    void COTRN01C_READ_TRANSACT_FILE_knownTransactionIsShown() throws Exception {
        Map<String, Object> row = jdbc.queryForMap(
                "SELECT * FROM transaction.daily_transactions WHERE tran_id = '0000000000683580'");
        mockMvc.perform(asUser(get("/api/v1/transactions/0000000000683580")))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.transactionId").value("0000000000683580"))
                .andExpect(jsonPath("$.typeCode").value("01"))
                .andExpect(jsonPath("$.categoryCode").value(1))
                .andExpect(jsonPath("$.source").value("POS TERM"))
                .andExpect(jsonPath("$.description").value("Purchase at Abshire-Lowe"))
                .andExpect(jsonPath("$.cardNumber").value(row.get("card_num")))
                .andExpect(jsonPath("$.merchantName").value(row.get("merchant_name")));
    }

    @Test
    @CobolFlow(program = "COTRN01C", paragraph = "READ-TRANSACT-FILE", key = ScreenKey.ENTER)
    void COTRN01C_READ_TRANSACT_FILE_unknownTransactionIsNotFound() throws Exception {
        mockMvc.perform(asUser(get("/api/v1/transactions/9999999999999999")))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("Transaction ID NOT found..."));
    }

    // ---- COTRN02C -------------------------------------------------------------------------------

    /** ADD-TRANSACTION: TRAN-ID = last TRAN-ID + 1 (STARTBR HIGH-VALUES, READPREV); then CT01 shows it. */
    @Test
    @CobolFlow(program = "COTRN02C", paragraph = "ADD-TRANSACTION", key = ScreenKey.ENTER)
    void COTRN02C_ADD_TRANSACTION_addByAccountThenViewIt() throws Exception {
        String last = jdbc.queryForObject("SELECT max(tran_id) FROM transaction.transactions", String.class);
        String expected = String.format("%016d", Long.parseLong(last) + 1);
        add(addScreen())
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.transactionId").value(expected))
                .andExpect(jsonPath("$.cardNumber").value("9680294154603697"))
                .andExpect(jsonPath("$.amount").value(-12.34));
        mockMvc.perform(asUser(get("/api/v1/transactions/" + expected)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.description").value("Flow test purchase"))
                .andExpect(jsonPath("$.merchantId").value("800000001"))
                .andExpect(jsonPath("$.originTimestamp").value("2022-07-18T00:00:00"));
    }

    @Test
    @CobolFlow(program = "COTRN02C", paragraph = "READ-CCXREF-FILE", key = ScreenKey.ENTER)
    void COTRN02C_READ_CCXREF_FILE_addByCardNumber() throws Exception {
        Map<String, String> body = addScreen();
        body.put("accountId", "");
        body.put("cardNumber", "0683586198171516");
        add(body).andExpect(status().isCreated()).andExpect(jsonPath("$.cardNumber").value("0683586198171516"));
        body.put("cardNumber", "1111222233334444");
        add(body).andExpect(status().isNotFound()).andExpect(jsonPath("$.message").value("Card Number NOT found..."));
    }

    @Test
    @CobolFlow(program = "COTRN02C", paragraph = "READ-CXACAIX-FILE", key = ScreenKey.ENTER)
    void COTRN02C_READ_CXACAIX_FILE_unknownAccountIsNotFound() throws Exception {
        Map<String, String> body = addScreen();
        body.put("accountId", "99999999999");
        add(body).andExpect(status().isNotFound()).andExpect(jsonPath("$.message").value("Account ID NOT found..."));
    }

    @Test
    @CobolFlow(program = "COTRN02C", paragraph = "VALIDATE-INPUT-KEY-FIELDS", key = ScreenKey.ENTER)
    void COTRN02C_VALIDATE_INPUT_KEY_FIELDS_everyKeyFieldMessage() throws Exception {
        String[][] cases = {
                {"accountId", "12X", "", "accountId", "Account ID must be Numeric..."},
                {"accountId", "", "06835861981715X6", "cardNumber", "Card Number must be Numeric..."},
                {"accountId", "", "", "accountId", "Account or Card Number must be entered..."}};
        for (String[] c : cases) {
            Map<String, String> body = addScreen();
            body.put("accountId", c[1]);
            body.put("cardNumber", c[2]);
            add(body).andExpect(status().isBadRequest())
                    .andExpect(jsonPath("$.fieldErrors[0].field").value(c[3]))
                    .andExpect(jsonPath("$.fieldErrors[0].message").value(c[4]));
        }
    }

    @Test
    @CobolFlow(program = "COTRN02C", paragraph = "VALIDATE-INPUT-DATA-FIELDS", key = ScreenKey.ENTER)
    void COTRN02C_VALIDATE_INPUT_DATA_FIELDS_everyDataFieldMessage() throws Exception {
        String[][] cases = {
                {"typeCode", "", "Type CD can NOT be empty..."},
                {"categoryCode", "", "Category CD can NOT be empty..."},
                {"source", "", "Source can NOT be empty..."},
                {"description", "", "Description can NOT be empty..."},
                {"amount", "", "Amount can NOT be empty..."},
                {"originDate", "", "Orig Date can NOT be empty..."},
                {"processDate", "", "Proc Date can NOT be empty..."},
                {"merchantId", "", "Merchant ID can NOT be empty..."},
                {"merchantName", "", "Merchant Name can NOT be empty..."},
                {"merchantCity", "", "Merchant City can NOT be empty..."},
                {"merchantZip", "", "Merchant Zip can NOT be empty..."},
                {"typeCode", "0X", "Type CD must be Numeric..."},
                {"categoryCode", "00X1", "Category CD must be Numeric..."},
                {"amount", "12.345", "Amount should be in format -99999999.99"},
                {"originDate", "2022/07/18", "Orig Date should be in format YYYY-MM-DD"},
                {"processDate", "18-07-2022", "Proc Date should be in format YYYY-MM-DD"},
                {"originDate", "2022-02-30", "Orig Date - Not a valid date..."},
                {"processDate", "2022-13-01", "Proc Date - Not a valid date..."},
                {"merchantId", "80000X001", "Merchant ID must be Numeric..."}};
        for (String[] c : cases) {
            Map<String, String> body = addScreen();
            body.put(c[0], c[1]);
            add(body).andExpect(status().isBadRequest())
                    .andExpect(jsonPath("$.fieldErrors[0].field").value(c[0]))
                    .andExpect(jsonPath("$.fieldErrors[0].message").value(c[2]));
        }
    }

    /** PF5 (COPY-LAST-TRAN-DATA) reads the last TRANSACT record; the API client reads the last list row. */
    @Test
    @CobolFlow(program = "COTRN02C", paragraph = "COPY-LAST-TRAN-DATA", key = ScreenKey.PF5)
    void COTRN02C_COPY_LAST_TRAN_DATA_lastRecordIsTheLastListRow() throws Exception {
        String last = jdbc.queryForObject("SELECT max(tran_id) FROM transaction.transactions", String.class);
        long total = jdbc.queryForObject("SELECT count(*) FROM transaction.transactions", Long.class);
        list("size", "1", "page", String.valueOf(total - 1))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].transactionId").value(last));
    }

    // ---- COBIL00C -------------------------------------------------------------------------------

    /**
     * Positive balance: the whole ACCT-CURR-BAL is paid as a type 02 / category 2 transaction and the
     * balance drops to zero; paying again finds a zero balance and answers COBIL00C's message.
     */
    @Test
    @CobolFlow(program = "COBIL00C", paragraph = "UPDATE-ACCTDAT-FILE", key = ScreenKey.ENTER)
    void COBIL00C_UPDATE_ACCTDAT_FILE_positiveBalanceIsPaidThenNothingIsLeftToPay() throws Exception {
        assertThat(balance(1)).isEqualByComparingTo("194.00");
        JsonNode payment = ServiceStandIn.JSON.readTree(pay("{\"accountId\":1}")
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.typeCode").value("02"))
                .andExpect(jsonPath("$.categoryCode").value(2))
                .andExpect(jsonPath("$.amount").value(194.00))
                .andExpect(jsonPath("$.cardNumber").value("9680294154603697"))
                .andExpect(jsonPath("$.description").value("BILL PAYMENT - ONLINE"))
                .andReturn().getResponse().getContentAsString());
        assertThat(balance(1)).isEqualByComparingTo("0.00");
        mockMvc.perform(asUser(get("/api/v1/transactions/" + payment.get("transactionId").asText())))
                .andExpect(status().isOk());
        pay("{\"accountId\":1}")
                .andExpect(status().isUnprocessableEntity())
                .andExpect(jsonPath("$.message").value("You have nothing to pay..."));
        assertThat(balance(1)).isEqualByComparingTo("0.00");
    }

    @Test
    @CobolFlow(program = "COBIL00C", paragraph = "PROCESS-ENTER-KEY", key = ScreenKey.ENTER)
    void COBIL00C_PROCESS_ENTER_KEY_zeroOrNegativeBalanceHasNothingToPay() throws Exception {
        jdbc.update("UPDATE account.accounts SET curr_bal = -5.00 WHERE acct_id = 2");
        pay("{\"accountId\":2}")
                .andExpect(status().isUnprocessableEntity())
                .andExpect(jsonPath("$.message").value("You have nothing to pay..."));
        pay("{}")
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].message").value("Acct ID can NOT be empty..."));
    }

    @Test
    @CobolFlow(program = "COBIL00C", paragraph = "READ-ACCTDAT-FILE", key = ScreenKey.ENTER)
    void COBIL00C_READ_ACCTDAT_FILE_unknownAccountIsNotFound() throws Exception {
        pay("{\"accountId\":99999999999}")
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("Account ID NOT found..."));
    }
}
