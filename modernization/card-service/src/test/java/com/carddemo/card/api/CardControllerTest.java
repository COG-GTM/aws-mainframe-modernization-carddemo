package com.carddemo.card.api;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.card.domain.Card;
import com.carddemo.card.domain.CardXref;
import com.carddemo.card.repository.CardRepository;
import com.carddemo.card.repository.CardXrefRepository;
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
class CardControllerTest {

    private static final String CARD_ONE = "4111111111111111";
    private static final String CARD_TWO = "4222222222222221";

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private CardRepository cards;

    @Autowired
    private CardXrefRepository xrefs;

    @BeforeEach
    void setUp() {
        xrefs.deleteAll();
        cards.deleteAll();
        Card first = new Card(CARD_ONE, 11111111111L);
        first.setEmbossedName("JOHN A DOE");
        first.setExpirationDate(LocalDate.of(2030, 1, 31));
        first.setCvvCd(123);
        Card second = new Card(CARD_TWO, 22222222222L);
        second.setEmbossedName("ALICE B ROE");
        second.setExpirationDate(LocalDate.of(2029, 6, 30));
        cards.save(first);
        cards.save(second);
        xrefs.save(new CardXref(CARD_ONE, 100000001L, 11111111111L));
        xrefs.save(new CardXref(CARD_TWO, 100000002L, 22222222222L));
    }

    @Test
    void returnsASingleCardWithoutTheCvv() throws Exception {
        mockMvc.perform(get("/api/v1/cards/{cardNumber}", CARD_ONE))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.cardNumber").value(CARD_ONE))
                .andExpect(jsonPath("$.accountId").value(11111111111L))
                .andExpect(jsonPath("$.cvv").doesNotExist());
    }

    @Test
    void listsCardsFilteredByAccountThroughTheCardAlternateIndex() throws Exception {
        mockMvc.perform(get("/api/v1/cards").param("accountId", "11111111111"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.totalElements").value(1))
                .andExpect(jsonPath("$.content[0].cardNumber").value(CARD_ONE));
    }

    @Test
    void listsCardsOfACustomerThroughTheCrossReference() throws Exception {
        mockMvc.perform(get("/api/v1/cards").param("customerId", "100000002"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.totalElements").value(1))
                .andExpect(jsonPath("$.content[0].cardNumber").value(CARD_TWO));
    }

    @Test
    void resolvesCardToAccountAndCustomer() throws Exception {
        mockMvc.perform(get("/api/v1/cards/{cardNumber}/xref", CARD_ONE))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.customerId").value(100000001L))
                .andExpect(jsonPath("$.accountId").value(11111111111L));

        mockMvc.perform(get("/api/v1/card-xrefs/by-account/{accountId}", 22222222222L))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.cardNumber").value(CARD_TWO));
    }

    @Test
    void returns404ForAnUnknownCard() throws Exception {
        mockMvc.perform(get("/api/v1/cards/{cardNumber}", "4000000000000000"))
                .andExpect(status().isNotFound());
    }

    @Test
    void updatesACard() throws Exception {
        mockMvc.perform(put("/api/v1/cards/{cardNumber}", CARD_ONE)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("""
                                {
                                  "embossedName": "John A Doe Jr",
                                  "expirationDate": "2031-01-31",
                                  "activeStatus": "N",
                                  "cvv": 321
                                }
                                """))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.embossedName").value("JOHN A DOE JR"))
                .andExpect(jsonPath("$.activeStatus").value("N"));
    }

    @Test
    void rejectsANonAlphabeticEmbossedName() throws Exception {
        mockMvc.perform(put("/api/v1/cards/{cardNumber}", CARD_ONE)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("""
                                {
                                  "embossedName": "J0HN 123",
                                  "expirationDate": "2031-01-31",
                                  "activeStatus": "Y"
                                }
                                """))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].field").value("embossedName"));
    }
}
