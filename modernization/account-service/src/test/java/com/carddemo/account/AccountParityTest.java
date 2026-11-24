package com.carddemo.account;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.common.testing.ParityTestSupport;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

/** Parity with COACTVWC (account view) and COACTUPC (account update). */
@SpringBootTest
@AutoConfigureMockMvc
class AccountParityTest extends ParityTestSupport {

    @Autowired
    MockMvc mvc;

    @Autowired
    ObjectMapper mapper;

    @Test
    void coactvwcJoinsAccountXrefAndCustomer() throws Exception {
        mvc.perform(get("/api/v1/accounts/00000000001").header("Authorization", userToken()))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.acctId").value(1))
                .andExpect(jsonPath("$.currBal").value(194.00))
                .andExpect(jsonPath("$.creditLimit").value(2020.00))
                .andExpect(jsonPath("$.custId").value(1))
                .andExpect(jsonPath("$.lastName").value("Kessler"))
                .andExpect(jsonPath("$.ficoCreditScore").value(274));
    }

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {
            "abc|400|Account number must be a non zero 11 digit number",
            "00000000000|400|Account number must be a non zero 11 digit number",
            "123456789012|400|Account number must be a non zero 11 digit number",
            "99999999999|404|Did not find this account in account card xref file"
    })
    void coactvwcErrors(String acct, int status, String message) throws Exception {
        mvc.perform(get("/api/v1/accounts/" + acct).header("Authorization", userToken()))
                .andExpect(status().is(status))
                .andExpect(jsonPath("$.message").value(message));
    }

    private ObjectNode current() throws Exception {
        String json = mvc.perform(get("/api/v1/accounts/1").header("Authorization", userToken()))
                .andReturn().getResponse().getContentAsString();
        ObjectNode node = (ObjectNode) mapper.readTree(json);
        // COACTUPC uses a valid FICO; fixture customer 1 carries the legacy out-of-range sample value 274.
        node.put("ficoCreditScore", 700);
        return node;
    }

    private org.springframework.test.web.servlet.ResultActions update(ObjectNode body) throws Exception {
        return mvc.perform(put("/api/v1/accounts/1").header("Authorization", userToken())
                .contentType(MediaType.APPLICATION_JSON).content(body.toString()));
    }

    @Test
    void coactupcCommitsChangesAndDetectsNoChange() throws Exception {
        ObjectNode body = current();
        body.put("creditLimit", 3000.00);
        update(body).andExpect(status().isOk())
                .andExpect(jsonPath("$.message").value("Changes committed to database"));
        mvc.perform(get("/api/v1/accounts/1").header("Authorization", userToken()))
                .andExpect(jsonPath("$.creditLimit").value(3000.00));
        update(current()).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("No change detected with respect to values fetched."));
    }

    @Test
    void coactupcRejectsStaleVersion() throws Exception {
        ObjectNode stale = current();
        ObjectNode first = current();
        first.put("creditLimit", 2500.00);
        update(first).andExpect(status().isOk());
        stale.put("creditLimit", 2600.00);
        update(stale).andExpect(status().isConflict())
                .andExpect(jsonPath("$.message").value("Record changed by some one else. Please review"));
    }

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {
            "activeStatus|X|Account Status must be Y or N.",
            "firstName|J0hn|First Name can have alphabets only.",
            "ficoCreditScore|900|FICO Score: should be between 300 and 850",
            "ssn|666123456|SSN: First 3 chars: should not be 000, 666, or between 900 and 999",
            "ssn|123004567|SSN 4th & 5th chars must not be zero.",
            "addrStateCd|ZZ|State: is not a valid state code",
            "addrZip|12A45|Zip must be all numeric.",
            "phoneNum1|908-119-8310|Phone Number 1 is not valid",
            "priCardHolderInd|Q|Primary Card Holder must be Y or N."
    })
    void coactupcFieldEdits(String field, String value, String message) throws Exception {
        ObjectNode body = current();
        if (field.equals("ficoCreditScore")) {
            body.put(field, Integer.parseInt(value));
        } else {
            body.put(field, value);
        }
        update(body).andExpect(status().isBadRequest()).andExpect(jsonPath("$.message").value(message));
    }
}
