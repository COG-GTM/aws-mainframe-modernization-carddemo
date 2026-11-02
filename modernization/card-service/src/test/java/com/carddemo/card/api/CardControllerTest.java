package com.carddemo.card.api;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.hasItem;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.card.domain.Card;
import com.carddemo.card.domain.CardXref;
import com.carddemo.card.repository.CardRepository;
import com.carddemo.card.repository.CardXrefRepository;
import com.carddemo.card.service.CardMessages;
import com.carddemo.common.error.RecordChangedException;
import com.carddemo.common.security.TokenService;
import com.carddemo.testsupport.AdminCaller;
import java.time.LocalDate;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;

/** Scenarios derived from COCRDLIC, COCRDSLC and COCRDUPC (no CICS runtime is available). */
@SpringBootTest
@AutoConfigureMockMvc
@Import(AdminCaller.class)
class CardControllerTest {

    private static final String CARD_ONE = "4111111111111111";
    private static final String CARD_TWO = "4222222222222221";
    private static final String CARD_THREE = "4333333333333331";
    private static final long ACCOUNT_ONE = 11111111111L;
    private static final long ACCOUNT_TWO = 22222222222L;

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private CardRepository cards;

    @Autowired
    private CardXrefRepository xrefs;

    @Autowired
    private TokenService tokens;

    @BeforeEach
    void setUp() {
        xrefs.deleteAll();
        cards.deleteAll();
        cards.save(card(CARD_ONE, ACCOUNT_ONE, "JOHN A DOE", LocalDate.of(2030, 1, 31), 123));
        cards.save(card(CARD_TWO, ACCOUNT_TWO, "ALICE B ROE", LocalDate.of(2029, 6, 30), 456));
        cards.save(card(CARD_THREE, ACCOUNT_ONE, "JANE DOE", LocalDate.of(2028, 3, 15), 789));
        xrefs.save(new CardXref(CARD_ONE, 100000001L, ACCOUNT_ONE));
        xrefs.save(new CardXref(CARD_TWO, 100000002L, ACCOUNT_TWO));
        xrefs.save(new CardXref(CARD_THREE, 100000001L, ACCOUNT_ONE));
    }

    private static Card card(String number, long account, String name, LocalDate expiry, int cvv) {
        Card card = new Card(number, account);
        card.setEmbossedName(name);
        card.setExpirationDate(expiry);
        card.setCvvCd(cvv);
        return card;
    }

    // ---- COCRDSLC -------------------------------------------------------------------------------

    @Test
    void returnsASingleCardWithoutTheCvv() throws Exception {
        mockMvc.perform(get("/api/v1/cards/{cardNumber}", CARD_ONE))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.cardNumber").value(CARD_ONE))
                .andExpect(jsonPath("$.accountId").value("11111111111"))
                .andExpect(jsonPath("$.embossedName").value("JOHN A DOE"))
                .andExpect(jsonPath("$.expirationDate").value("2030-01-31"))
                .andExpect(jsonPath("$.activeStatus").value("Y"))
                .andExpect(jsonPath("$.version").value(0))
                .andExpect(jsonPath("$.cvv").doesNotExist())
                .andExpect(jsonPath("$.cvvCd").doesNotExist());
    }

    @Test
    void answersAnUnknownCardWithTheCocrdslcNotFoundMessage() throws Exception {
        mockMvc.perform(get("/api/v1/cards/{cardNumber}", "4000000000000000"))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value(CardMessages.NOT_FOUND));
    }

    @Test
    void editsTheCardNumberKeyLike2220EditCard() throws Exception {
        mockMvc.perform(get("/api/v1/cards/{cardNumber}", "41111111111A1111"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].field").value("cardNumber"))
                .andExpect(jsonPath("$.fieldErrors[0].message").value(CardMessages.CARD_FILTER_NOT_NUMERIC));
        mockMvc.perform(get("/api/v1/cards/{cardNumber}", "0000000000000000"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].message").value(CardMessages.CARD_NOT_PROVIDED));
    }

    // ---- COCRDLIC -------------------------------------------------------------------------------

    @Test
    void listsAllCardsInCardNumberOrderWithoutFilters() throws Exception {
        mockMvc.perform(get("/api/v1/cards"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.totalElements").value(3))
                .andExpect(jsonPath("$.content[*].cardNumber").value(contains(CARD_ONE, CARD_TWO, CARD_THREE)));
    }

    @Test
    void filtersByAccount() throws Exception {
        mockMvc.perform(get("/api/v1/cards").param("accountId", "11111111111"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content[*].cardNumber").value(contains(CARD_ONE, CARD_THREE)));
    }

    @Test
    void filtersByCardNumber() throws Exception {
        mockMvc.perform(get("/api/v1/cards").param("cardNumber", CARD_TWO))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.totalElements").value(1))
                .andExpect(jsonPath("$.content[0].cardNumber").value(CARD_TWO));
    }

    @Test
    void intersectsAccountAndCardNumberFiltersLike9500FilterRecords() throws Exception {
        mockMvc.perform(get("/api/v1/cards").param("accountId", "11111111111").param("cardNumber", CARD_THREE))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.totalElements").value(1))
                .andExpect(jsonPath("$.content[0].cardNumber").value(CARD_THREE));
        mockMvc.perform(get("/api/v1/cards").param("accountId", "11111111111").param("cardNumber", CARD_TWO))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.totalElements").value(0));
    }

    @Test
    void treatsBlankAndAllZeroFiltersAsNotSupplied() throws Exception {
        mockMvc.perform(get("/api/v1/cards").param("accountId", "00000000000").param("cardNumber", ""))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.totalElements").value(3));
    }

    @Test
    void rejectsNonNumericFiltersWithTheCocrdlicMessages() throws Exception {
        mockMvc.perform(get("/api/v1/cards").param("accountId", "1111"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].field").value("accountId"))
                .andExpect(jsonPath("$.fieldErrors[0].message").value(CardMessages.ACCOUNT_FILTER_NOT_NUMERIC));
        mockMvc.perform(get("/api/v1/cards").param("cardNumber", "4111-1111"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].field").value("cardNumber"))
                .andExpect(jsonPath("$.fieldErrors[0].message").value(CardMessages.CARD_FILTER_NOT_NUMERIC));
    }

    @Test
    void listsCardsOfACustomerThroughTheCrossReferenceIntersectedWithTheOtherFilters() throws Exception {
        mockMvc.perform(get("/api/v1/cards").param("customerId", "100000001"))
                .andExpect(jsonPath("$.content[*].cardNumber").value(contains(CARD_ONE, CARD_THREE)));
        mockMvc.perform(get("/api/v1/cards").param("customerId", "100000001").param("cardNumber", CARD_THREE))
                .andExpect(jsonPath("$.totalElements").value(1));
        mockMvc.perform(get("/api/v1/cards").param("customerId", "100000001").param("accountId", "22222222222"))
                .andExpect(jsonPath("$.totalElements").value(0));
    }

    @Test
    void pagesSevenRowsAtATimeInCardNumberOrderLikePf7Pf8() throws Exception {
        xrefs.deleteAll();
        cards.deleteAll();
        // inserted out of key order on purpose; the browse reads CARDDAT in key order
        for (int i = 9; i >= 1; i--) {
            cards.save(card("40000000000000" + String.format("%02d", i), ACCOUNT_ONE, "CARD HOLDER", LocalDate.of(2030, 1, 31), 1));
        }
        mockMvc.perform(get("/api/v1/cards").param("size", "7"))
                .andExpect(jsonPath("$.totalElements").value(9))
                .andExpect(jsonPath("$.totalPages").value(2))
                .andExpect(jsonPath("$.content[*].cardNumber").value(contains(
                        "4000000000000001", "4000000000000002", "4000000000000003", "4000000000000004",
                        "4000000000000005", "4000000000000006", "4000000000000007")));
        mockMvc.perform(get("/api/v1/cards").param("size", "7").param("page", "1"))
                .andExpect(jsonPath("$.content[*].cardNumber").value(contains("4000000000000008", "4000000000000009")));
    }

    @Test
    void showsAUserTheSameCardsAsAnAdminBecauseCocrdlicHasNoUserTypeFilter() throws Exception {
        mockMvc.perform(get("/api/v1/cards").header(HttpHeaders.AUTHORIZATION, "Bearer " + tokens.issue("USER0001", "U").token()))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.totalElements").value(3));
    }

    // ---- COCRDUPC -------------------------------------------------------------------------------

    private ResultActions update(String cardNumber, String body) throws Exception {
        return mockMvc.perform(put("/api/v1/cards/{cardNumber}", cardNumber)
                .contentType(MediaType.APPLICATION_JSON)
                .content(body));
    }

    private static String body(String name, String status, Object month, Object year, long version) {
        return """
                {"embossedName": %s, "activeStatus": %s, "expirationMonth": %s, "expirationYear": %s, "version": %d}
                """.formatted(json(name), json(status), month, year, version);
    }

    private static String json(String value) {
        return value == null ? "null" : "\"" + value + "\"";
    }

    @Test
    void updatesNameStatusAndExpiryKeepingDayCvvAndAccount() throws Exception {
        update(CARD_ONE, body("John A Doe Jr", "N", 2, 2031, 0))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.embossedName").value("John A Doe Jr"))
                .andExpect(jsonPath("$.activeStatus").value("N"))
                // stored day 31 does not exist in February: last day of the month (hypothesis, see CardService)
                .andExpect(jsonPath("$.expirationDate").value("2031-02-28"))
                .andExpect(jsonPath("$.accountId").value("11111111111"))
                .andExpect(jsonPath("$.version").value(1));

        Card stored = cards.findById(CARD_ONE).orElseThrow();
        assertThat(stored.getCvvCd()).isEqualTo(123);
        assertThat(stored.getAcctId()).isEqualTo(ACCOUNT_ONE);
    }

    @Test
    void keepsTheStoredExpiryDay() throws Exception {
        update(CARD_THREE, body("JANE DOE", "Y", 11, 2032, 0))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.expirationDate").value("2032-11-15"));
    }

    @Test
    void writesNothingWhenNothingChangedIgnoringCase() throws Exception {
        update(CARD_ONE, body("john a doe", "Y", 1, 2030, 0))
                .andExpect(status().isUnprocessableEntity())
                .andExpect(jsonPath("$.message").value(CardMessages.NO_CHANGES_DETECTED));
        assertThat(cards.findById(CARD_ONE).orElseThrow().getVersion()).isZero();
    }

    @Test
    void rejectsARewriteWhenTheRecordChangedSinceItWasRead() throws Exception {
        update(CARD_ONE, body("JOHN DOE", "Y", 1, 2030, 0)).andExpect(status().isOk());
        update(CARD_ONE, body("JOHNNY DOE", "Y", 1, 2030, 0))
                .andExpect(status().isConflict())
                .andExpect(jsonPath("$.message").value(RecordChangedException.MESSAGE));
        assertThat(cards.findById(CARD_ONE).orElseThrow().getEmbossedName()).isEqualTo("JOHN DOE");
    }

    @Test
    void answersAnUnknownCardOnUpdateWithTheNotFoundMessage() throws Exception {
        update("4000000000000000", body("JOHN DOE", "Y", 1, 2030, 0))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value(CardMessages.NOT_FOUND));
    }

    @Test
    void editsTheNameLike1230EditName() throws Exception {
        update(CARD_ONE, body("J0HN 123", "Y", 1, 2031, 0))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].field").value("embossedName"))
                .andExpect(jsonPath("$.fieldErrors[0].message").value(CardMessages.NAME_MUST_BE_ALPHA));
        update(CARD_ONE, body("O'BRIEN", "Y", 1, 2031, 0))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].message").value(CardMessages.NAME_MUST_BE_ALPHA));
        update(CARD_ONE, body("   ", "Y", 1, 2031, 0))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].message").value(CardMessages.NAME_NOT_PROVIDED));
    }

    @Test
    void editsTheStatusLike1240EditCardStatus() throws Exception {
        update(CARD_ONE, body("JOHN DOE", "X", 1, 2031, 0))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].field").value("activeStatus"))
                .andExpect(jsonPath("$.fieldErrors[0].message").value(CardMessages.STATUS_MUST_BE_YES_NO));
        update(CARD_ONE, body("JOHN DOE", null, 1, 2031, 0))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].message").value(CardMessages.STATUS_MUST_BE_YES_NO));
    }

    @Test
    void editsTheExpiryMonthAndYearLike1250And1260() throws Exception {
        update(CARD_ONE, body("JOHN DOE", "Y", 13, 2031, 0))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].field").value("expirationMonth"))
                .andExpect(jsonPath("$.fieldErrors[0].message").value(CardMessages.EXPIRY_MONTH_NOT_VALID));
        update(CARD_ONE, body("JOHN DOE", "Y", 0, 2031, 0))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].message").value(CardMessages.EXPIRY_MONTH_NOT_VALID));
        update(CARD_ONE, body("JOHN DOE", "Y", 1, 1949, 0))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].field").value("expirationYear"))
                .andExpect(jsonPath("$.fieldErrors[0].message").value(CardMessages.EXPIRY_YEAR_NOT_VALID));
        update(CARD_ONE, body("JOHN DOE", "Y", 1, 2100, 0))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[*].message").value(hasItem(CardMessages.EXPIRY_YEAR_NOT_VALID)));
        update(CARD_ONE, body("JOHN DOE", "Y", 12, 1950, 0)).andExpect(status().isOk());
    }

    @Test
    void editsTheCardNumberKeyOnUpdate() throws Exception {
        update("0000000000000000", body("JOHN DOE", "Y", 1, 2031, 0))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].message").value(CardMessages.CARD_NOT_PROVIDED));
    }
}
