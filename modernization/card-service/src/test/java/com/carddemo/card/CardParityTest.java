package com.carddemo.card;

import static org.hamcrest.Matchers.hasSize;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.common.testing.ParityTestSupport;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.web.servlet.MockMvc;

/** Parity with COCRDLIC, COCRDSLC and COCRDUPC. */
@SpringBootTest
@AutoConfigureMockMvc
class CardParityTest extends ParityTestSupport {

    @Autowired
    MockMvc mvc;

    @Autowired
    JdbcTemplate jdbc;

    @Test
    void cocrdlicPagesSevenCardsAndFiltersByAccountAix() throws Exception {
        for (int i = 0; i < 6; i++) {
            jdbc.update("insert into card (card_num, acct_id, cvv_cd, embossed_name, expiration_date, active_status)"
                    + " values (?, 2, '000', 'Extra Card', '2099-01-01', 'Y')", "50000000000000" + String.format("%02d", i));
        }
        mvc.perform(get("/api/v1/cards").header("Authorization", userToken()))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.items", hasSize(7)))
                .andExpect(jsonPath("$.hasNextPage").value(true))
                .andExpect(jsonPath("$.items[0].cardNum").value("4111111111111111"));
        mvc.perform(get("/api/v1/cards?after=5000000000000002&page=1").header("Authorization", userToken()))
                .andExpect(jsonPath("$.items", hasSize(3)))
                .andExpect(jsonPath("$.hasNextPage").value(false));
        mvc.perform(get("/api/v1/cards?after=5000000000000005&page=2").header("Authorization", userToken()))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("NO MORE PAGES TO DISPLAY"));
        mvc.perform(get("/api/v1/cards?acctId=00000000001").header("Authorization", userToken()))
                .andExpect(jsonPath("$.items", hasSize(2)))
                .andExpect(jsonPath("$.items[1].cardNum").value("4111111111111112"));
    }

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {
            "acctId=12345|400|ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER",
            "cardNum=12AB|400|CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER",
            "acctId=00000000099|404|NO RECORDS FOUND FOR THIS SEARCH CONDITION."
    })
    void cocrdlicFilterErrors(String query, int status, String message) throws Exception {
        mvc.perform(get("/api/v1/cards?" + query).header("Authorization", userToken()))
                .andExpect(status().is(status))
                .andExpect(jsonPath("$.message").value(message));
    }

    @Test
    void cocrdslcViewAndNotFound() throws Exception {
        mvc.perform(get("/api/v1/cards/4222222222222222").header("Authorization", userToken()))
                .andExpect(jsonPath("$.acctId").value(2))
                .andExpect(jsonPath("$.embossedName").value("Enrico Rosenbaum"))
                .andExpect(jsonPath("$.cvvCd").doesNotExist());
        mvc.perform(get("/api/v1/cards/4222222222222222?acctId=00000000001").header("Authorization", userToken()))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("Did not find cards for this search condition"));
    }

    private org.springframework.test.web.servlet.ResultActions update(String body) throws Exception {
        return mvc.perform(put("/api/v1/cards/4222222222222222").header("Authorization", userToken())
                .contentType(MediaType.APPLICATION_JSON).content(body));
    }

    @Test
    void cocrdupcUpdatesKeepsDayAndDetectsNoChange() throws Exception {
        update("{\"embossedName\":\"Enrico R Rosenbaum\",\"activeStatus\":\"N\",\"expiryMonth\":2,"
                + "\"expiryYear\":2030,\"version\":0}")
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.message").value("Changes committed to database"));
        mvc.perform(get("/api/v1/cards/4222222222222222").header("Authorization", userToken()))
                .andExpect(jsonPath("$.expirationDate").value("2030-02-13"))
                .andExpect(jsonPath("$.activeStatus").value("N"))
                .andExpect(jsonPath("$.version").value(1));
        update("{\"embossedName\":\"Enrico R Rosenbaum\",\"activeStatus\":\"N\",\"expiryMonth\":2,"
                + "\"expiryYear\":2030,\"version\":1}")
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("No change detected with respect to values fetched."));
        update("{\"embossedName\":\"Someone Else\",\"activeStatus\":\"Y\",\"expiryMonth\":2,"
                + "\"expiryYear\":2030,\"version\":0}")
                .andExpect(status().isConflict())
                .andExpect(jsonPath("$.message").value("Record changed by some one else. Please review"));
    }

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {
            "{\"embossedName\":\"R2D2\",\"activeStatus\":\"Y\",\"expiryMonth\":1,\"expiryYear\":2030,\"version\":0}|Card name can only contain alphabets and spaces",
            "{\"embossedName\":\"Ann\",\"activeStatus\":\"X\",\"expiryMonth\":1,\"expiryYear\":2030,\"version\":0}|Card Active Status must be Y or N",
            "{\"embossedName\":\"Ann\",\"activeStatus\":\"Y\",\"expiryMonth\":13,\"expiryYear\":2030,\"version\":0}|Card expiry month must be between 1 and 12",
            "{\"embossedName\":\"Ann\",\"activeStatus\":\"Y\",\"expiryMonth\":1,\"expiryYear\":2100,\"version\":0}|Invalid card expiry year"
    })
    void cocrdupcEdits(String body, String message) throws Exception {
        update(body).andExpect(status().isBadRequest()).andExpect(jsonPath("$.message").value(message));
    }
}
