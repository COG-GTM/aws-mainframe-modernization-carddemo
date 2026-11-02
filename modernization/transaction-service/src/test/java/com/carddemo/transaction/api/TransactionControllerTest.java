package com.carddemo.transaction.api;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.matchesPattern;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.testsupport.AdminCaller;
import com.carddemo.transaction.client.PostingResult;
import com.carddemo.transaction.domain.Transaction;
import com.carddemo.transaction.repository.TransactionRepository;
import com.carddemo.transaction.support.StubGateways;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;

/** Source-derived scenarios for COTRN00C, COTRN01C, COTRN02C and COBIL00C. */
@SpringBootTest
@AutoConfigureMockMvc
@Import({StubGateways.class, AdminCaller.class})
class TransactionControllerTest {

    private static final String CARD = "4111111111111111";
    private static final long ACCOUNT = 11111111111L;
    private static final String VALID_ADD = """
            {
              "cardNumber": "4111111111111111",
              "typeCode": "01",
              "categoryCode": "0001",
              "source": "POS TERM",
              "description": "GROCERIES",
              "amount": "-00000042.50",
              "originDate": "2024-05-01",
              "processDate": "2024-05-02",
              "merchantId": "000000123",
              "merchantName": "CORNER SHOP",
              "merchantCity": "AUSTIN",
              "merchantZip": "78701"
            }
            """;

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ObjectMapper json;

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

    private ResultActions add(String body) throws Exception {
        return mockMvc.perform(post("/api/v1/transactions")
                .contentType(MediaType.APPLICATION_JSON).content(body));
    }

    private ResultActions payBill(long accountId) throws Exception {
        return mockMvc.perform(post("/api/v1/bill-payments")
                .contentType(MediaType.APPLICATION_JSON).content("{\"accountId\": " + accountId + "}"));
    }

    private String with(String field, String value) throws Exception {
        com.fasterxml.jackson.databind.node.ObjectNode node =
                (com.fasterxml.jackson.databind.node.ObjectNode) json.readTree(VALID_ADD);
        if (value == null) {
            node.remove(field);
        } else {
            node.put(field, value);
        }
        return json.writeValueAsString(node);
    }

    private void store(String tranId) {
        Transaction transaction = new Transaction(tranId, CARD, "01", 1, BigDecimal.ONE);
        transaction.setOrigTs(LocalDateTime.of(2024, 1, 1, 0, 0));
        transaction.setProcTs(LocalDateTime.of(2024, 1, 1, 0, 0));
        transactions.save(transaction);
    }

    // --- COTRN02C -----------------------------------------------------------------------------

    @Test
    void addsATransactionWithTheTypedDatesAndANumericId() throws Exception {
        add(VALID_ADD)
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.transactionId").value(matchesPattern("\\d{16}")))
                .andExpect(jsonPath("$.cardNumber").value(CARD))
                .andExpect(jsonPath("$.typeCode").value("01"))
                .andExpect(jsonPath("$.categoryCode").value(1))
                .andExpect(jsonPath("$.amount").value(-42.50))
                .andExpect(jsonPath("$.merchantId").value("000000123"))
                .andExpect(jsonPath("$.originTimestamp").value("2024-05-01T00:00:00"))
                .andExpect(jsonPath("$.processTimestamp").value("2024-05-02T00:00:00"));

        assertThat(transactions.count()).isEqualTo(1);
    }

    @Test
    void acceptsJsonNumbersForTheNumericFields() throws Exception {
        add("""
                {"cardNumber": 4111111111111111, "typeCode": 1, "categoryCode": 1, "source": "POS TERM",
                 "description": "X", "amount": 12.5, "originDate": "2024-05-01", "processDate": "2024-05-01",
                 "merchantId": 7, "merchantName": "M", "merchantCity": "C", "merchantZip": "Z"}
                """)
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.typeCode").value("01"))
                .andExpect(jsonPath("$.amount").value(12.50));
    }

    @Test
    void anAccountIdResolvesTheCardThroughCxacaixAndWinsOverACardNumber() throws Exception {
        cards.register("4222222222222222", 100000002L, 22222222222L);

        add(with("accountId", "22222222222"))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.cardNumber").value("4222222222222222"));
    }

    @Test
    void transactionIdIsTheHighestStoredIdPlusOne() throws Exception {
        store("0000000001774260");
        store("ABCDEFGHIJKLMNOP");

        String first = idOf(add(VALID_ADD).andExpect(status().isCreated()));
        String second = idOf(add(VALID_ADD).andExpect(status().isCreated()));

        assertThat(first).isEqualTo("0000000001774261");
        assertThat(second).isEqualTo("0000000001774262");
    }

    @Test
    void transactionIdsKeepIncreasingWhenTheHighestRowIsRemoved() throws Exception {
        String first = idOf(add(VALID_ADD).andExpect(status().isCreated()));
        transactions.deleteAll();
        String second = idOf(add(VALID_ADD).andExpect(status().isCreated()));

        assertThat(Long.parseLong(second)).isGreaterThan(Long.parseLong(first));
    }

    private String idOf(ResultActions result) throws Exception {
        JsonNode body = json.readTree(result.andReturn().getResponse().getContentAsString());
        return body.get("transactionId").asText();
    }

    @Test
    void anEmptyRequestReportsTheCobolMessagesInCobolOrder() throws Exception {
        add("{}")
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Account or Card Number must be entered..."))
                .andExpect(jsonPath("$.fieldErrors[0].field").value("accountId"))
                .andExpect(jsonPath("$.fieldErrors[1].field").value("typeCode"))
                .andExpect(jsonPath("$.fieldErrors[1].message").value("Type CD can NOT be empty..."))
                .andExpect(jsonPath("$.fieldErrors[2].message").value("Category CD can NOT be empty..."))
                .andExpect(jsonPath("$.fieldErrors[3].message").value("Source can NOT be empty..."))
                .andExpect(jsonPath("$.fieldErrors[4].message").value("Description can NOT be empty..."))
                .andExpect(jsonPath("$.fieldErrors[5].message").value("Amount can NOT be empty..."))
                .andExpect(jsonPath("$.fieldErrors[6].message").value("Orig Date can NOT be empty..."))
                .andExpect(jsonPath("$.fieldErrors[7].message").value("Proc Date can NOT be empty..."))
                .andExpect(jsonPath("$.fieldErrors[8].message").value("Merchant ID can NOT be empty..."))
                .andExpect(jsonPath("$.fieldErrors[9].message").value("Merchant Name can NOT be empty..."))
                .andExpect(jsonPath("$.fieldErrors[10].message").value("Merchant City can NOT be empty..."))
                .andExpect(jsonPath("$.fieldErrors[11].message").value("Merchant Zip can NOT be empty..."));
        assertThat(transactions.count()).isZero();
    }

    @Test
    void rejectsNonNumericKeysAndCodesWithTheCobolMessages() throws Exception {
        add(with("accountId", "12AB"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Account ID must be Numeric..."));
        add(with("cardNumber", "4111-1111"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Card Number must be Numeric..."));
        add(with("typeCode", "A1"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Type CD must be Numeric..."));
        add(with("categoryCode", "12345"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Category CD must be Numeric..."));
        add(with("merchantId", "M1"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Merchant ID must be Numeric..."));
    }

    @Test
    void rejectsAnAmountOutsideTheMinus99999999Point99Format() throws Exception {
        for (String amount : new String[] {"123456789.00", "1.234", "12,50", "abc"}) {
            add(with("amount", amount))
                    .andExpect(status().isBadRequest())
                    .andExpect(jsonPath("$.message").value("Amount should be in format -99999999.99"))
                    .andExpect(jsonPath("$.fieldErrors[0].field").value("amount"));
        }
    }

    @Test
    void checksTheDateFormatThenTheDateThroughCsutldtc() throws Exception {
        add(with("originDate", "05/01/2024"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Orig Date should be in format YYYY-MM-DD"));
        add(with("processDate", "2024-5-1"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Proc Date should be in format YYYY-MM-DD"));
        add(with("originDate", "2023-02-29"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Orig Date - Not a valid date..."));
        add(with("processDate", "2024-13-01"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Proc Date - Not a valid date..."));
        add(with("originDate", "2024-02-29")).andExpect(status().isCreated());
    }

    @Test
    void anUnknownCardOrAccountIsNotFoundWithTheCobolMessage() throws Exception {
        add(with("cardNumber", "4999999999999999"))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("Card Number NOT found..."));
        add(with("accountId", "99999999999"))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("Account ID NOT found..."));
        assertThat(transactions.count()).isZero();
    }

    // --- COTRN00C / COTRN01C ------------------------------------------------------------------

    @Test
    void listsInTransactionIdOrderFromTheTypedStartId() throws Exception {
        store("0000000000000010");
        store("0000000000000020");
        store("0000000000000030");

        mockMvc.perform(get("/api/v1/transactions").param("startTransactionId", "20").param("size", "10"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.totalElements").value(2))
                .andExpect(jsonPath("$.content[0].transactionId").value("0000000000000020"))
                .andExpect(jsonPath("$.content[1].transactionId").value("0000000000000030"));

        mockMvc.perform(get("/api/v1/transactions").param("size", "2").param("page", "1"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[0].transactionId").value("0000000000000030"));

        mockMvc.perform(get("/api/v1/transactions").param("startTransactionId", "1A"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].field").value("startTransactionId"))
                .andExpect(jsonPath("$.fieldErrors[0].message").value("Tran ID must be Numeric ..."));
    }

    @Test
    void listsTransactionsFilteredByCard() throws Exception {
        store("0000000000000010");
        mockMvc.perform(get("/api/v1/transactions").param("cardNumber", CARD))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.totalElements").value(1));
        mockMvc.perform(get("/api/v1/transactions").param("cardNumber", "4999999999999999"))
                .andExpect(jsonPath("$.totalElements").value(0));
    }

    @Test
    void rejectsAnInvalidPageSize() throws Exception {
        mockMvc.perform(get("/api/v1/transactions").param("size", "0"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].field").value("size"));

        mockMvc.perform(get("/api/v1/transactions").param("page", "-1"))
                .andExpect(status().isBadRequest());
    }

    @Test
    void viewsATransactionOrAnswersTheCobolNotFoundMessage() throws Exception {
        store("0000000000000010");
        mockMvc.perform(get("/api/v1/transactions/{id}", "0000000000000010"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.cardNumber").value(CARD));
        mockMvc.perform(get("/api/v1/transactions/{id}", "0000000000000000"))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("Transaction ID NOT found..."));
    }

    // --- COBIL00C -----------------------------------------------------------------------------

    @Test
    void paysTheWholeBalanceKeyedByTheNewTransactionId() throws Exception {
        store("0000000000000041");

        payBill(ACCOUNT)
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.transactionId").value("0000000000000042"))
                .andExpect(jsonPath("$.amount").value(250.00))
                .andExpect(jsonPath("$.typeCode").value("02"))
                .andExpect(jsonPath("$.categoryCode").value(2))
                .andExpect(jsonPath("$.source").value("POS TERM"))
                .andExpect(jsonPath("$.description").value("BILL PAYMENT - ONLINE"))
                .andExpect(jsonPath("$.merchantId").value("999999999"))
                .andExpect(jsonPath("$.merchantName").value("BILL PAYMENT"))
                .andExpect(jsonPath("$.merchantCity").value("N/A"))
                .andExpect(jsonPath("$.merchantZip").value("N/A"))
                .andExpect(jsonPath("$.cardNumber").value(CARD));

        assertThat(accounts.postings()).containsExactly(new BigDecimal("-250.00"));
        assertThat(accounts.billPaymentKeys()).containsExactly("0000000000000042");
        assertThat(accounts.balance(ACCOUNT)).isEqualByComparingTo("0.00");
    }

    @Test
    void aSecondPaymentFindsNothingToPay() throws Exception {
        payBill(ACCOUNT).andExpect(status().isCreated());
        payBill(ACCOUNT)
                .andExpect(status().isUnprocessableEntity())
                .andExpect(jsonPath("$.message").value("You have nothing to pay..."));
        assertThat(transactions.count()).isEqualTo(1);
    }

    @Test
    void rejectsAZeroOrNegativeBalanceWithTheCobolMessage() throws Exception {
        accounts.register(ACCOUNT, "DEFAULT", new BigDecimal("-5.00"));
        payBill(ACCOUNT)
                .andExpect(status().isUnprocessableEntity())
                .andExpect(jsonPath("$.message").value("You have nothing to pay..."));
        assertThat(accounts.postings()).isEmpty();
        assertThat(transactions.count()).isZero();
    }

    @Test
    void anUnknownOrMissingAccountIdUsesTheCobolMessages() throws Exception {
        payBill(99999999999L)
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("Account ID NOT found..."));
        mockMvc.perform(post("/api/v1/bill-payments").contentType(MediaType.APPLICATION_JSON).content("{}"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].message").value("Acct ID can NOT be empty..."));
    }

    @Test
    void aLostPostingAnswerIsRetriedWithTheSameIdAndDebitsOnce() throws Exception {
        accounts.loseAnswers(1, true);

        payBill(ACCOUNT).andExpect(status().isCreated());

        assertThat(accounts.billPaymentKeys()).hasSize(2);
        assertThat(accounts.billPaymentKeys().get(0)).isEqualTo(accounts.billPaymentKeys().get(1));
        assertThat(accounts.balance(ACCOUNT)).isEqualByComparingTo("0.00");
        assertThat(transactions.count()).isEqualTo(1);
    }

    @Test
    void anUnreachableAccountServiceLeavesNoTransaction() throws Exception {
        accounts.loseAnswers(3, false);

        payBill(ACCOUNT).andExpect(status().isInternalServerError());

        assertThat(accounts.billPaymentKeys()).hasSize(3).containsOnly(accounts.billPaymentKeys().get(0));
        assertThat(accounts.balance(ACCOUNT)).isEqualByComparingTo("250.00");
        assertThat(transactions.count()).isZero();
    }

    @Test
    void doesNotRecordABillPaymentTheAccountRejected() throws Exception {
        accounts.forceResult(ACCOUNT, new PostingResult(false, 101, "ACCOUNT RECORD NOT FOUND", null, null, null));

        payBill(ACCOUNT)
                .andExpect(status().isUnprocessableEntity())
                .andExpect(jsonPath("$.message").value(org.hamcrest.Matchers.containsString("101")));

        assertThat(transactions.count()).isZero();
    }

    @Test
    void aBalanceTooWideForTranAmtFailsBeforeTheAccountIsDebited() throws Exception {
        accounts.register(ACCOUNT, "DEFAULT", new BigDecimal("1000000000.00"));

        payBill(ACCOUNT).andExpect(status().isInternalServerError());

        assertThat(accounts.postings()).isEmpty();
        assertThat(transactions.count()).isZero();
    }
}
