package com.carddemo;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.junit.jupiter.api.Test;

class AccountIntegrationTest extends IntegrationTestBase {

    private ObjectNode editableAccount(String token) throws Exception {
        ObjectNode view = (ObjectNode) body(mvc.perform(as(token, get("/api/v1/accounts/00000000050")))
                .andExpect(status().isOk()).andReturn());
        ObjectNode customer = (ObjectNode) view.get("customer");
        customer.put("addrStateCd", "NY");
        customer.put("addrZip", "10001");
        customer.put("phoneNum1", "(212)555-1234");
        customer.put("phoneNum2", "(718)555-6789");
        customer.put("ssn", "123456789");
        return view;
    }

    @Test
    void viewReturnsAccountCustomerAndCardsViaXref() throws Exception {
        mvc.perform(as(userToken(), get("/api/v1/accounts/50")))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.acctId").value(50))
                .andExpect(jsonPath("$.activeStatus").value("Y"))
                .andExpect(jsonPath("$.currBal").value("492.00"))
                .andExpect(jsonPath("$.customer.custId").value(50))
                .andExpect(jsonPath("$.customer.firstName").value("Aniya"))
                .andExpect(jsonPath("$.cards[0].cardNum").value("0500024453765740"))
                .andExpect(jsonPath("$.version").value(0));
    }

    @Test
    void viewRejectsNonNumericAccount() throws Exception {
        mvc.perform(as(userToken(), get("/api/v1/accounts/ABC")))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.legacyProgram").value("COACTVWC"))
                .andExpect(jsonPath("$.message").value("Account number must be a non zero 11 digit number"));
    }

    @Test
    void viewReportsUnknownAccount() throws Exception {
        mvc.perform(as(userToken(), get("/api/v1/accounts/99999999999")))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.errorCode").value("NOT_FOUND"))
                .andExpect(jsonPath("$.message").value("Did not find this account in account card xref file"));
    }

    @Test
    void updateCommitsAccountAndCustomerChanges() throws Exception {
        String token = userToken();
        ObjectNode request = editableAccount(token);
        request.put("creditLimit", "7000.00");
        ((ObjectNode) request.get("customer")).put("ficoCreditScore", "720");
        JsonNode updated = body(mvc.perform(as(token, withJson(put("/api/v1/accounts/50"), request)))
                .andExpect(status().isOk()).andReturn());
        assertThat(updated.get("creditLimit").asText()).isEqualTo("7000.00");
        assertThat(updated.at("/customer/ficoCreditScore").asInt()).isEqualTo(720);
        assertThat(updated.at("/customer/addrStateCd").asText()).isEqualTo("NY");
        assertThat(updated.get("version").asLong()).isEqualTo(1);
        assertThat(updated.get("message").asText()).isEqualTo("Changes committed to database");
    }

    @Test
    void updateWithStaleVersionIsConflict() throws Exception {
        String token = userToken();
        ObjectNode request = editableAccount(token);
        request.put("creditLimit", "7000.00");
        mvc.perform(as(token, withJson(put("/api/v1/accounts/50"), request))).andExpect(status().isOk());
        request.put("creditLimit", "8000.00");
        mvc.perform(as(token, withJson(put("/api/v1/accounts/50"), request)))
                .andExpect(status().isConflict())
                .andExpect(jsonPath("$.errorCode").value("CONCURRENT_UPDATE"))
                .andExpect(jsonPath("$.message").value("Record changed by some one else. Please review"));
    }

    @Test
    void updateWithoutChangesIsRejected() throws Exception {
        String token = userToken();
        JsonNode view = body(mvc.perform(as(token, get("/api/v1/accounts/50"))).andReturn());
        mvc.perform(as(token, withJson(put("/api/v1/accounts/50"), view)))
                .andExpect(status().isUnprocessableEntity())
                .andExpect(jsonPath("$.message").value("No change detected with respect to values fetched."));
    }

    @Test
    void updateAppliesCslkpcdyAndFicoEdits() throws Exception {
        String token = userToken();
        ObjectNode request = editableAccount(token);
        ObjectNode customer = (ObjectNode) request.get("customer");
        customer.put("addrStateCd", "XX");
        customer.put("ficoCreditScore", "900");
        customer.put("ssn", "666123456");
        customer.put("phoneNum1", "(000)555-1234");
        JsonNode error = body(mvc.perform(as(token, withJson(put("/api/v1/accounts/50"), request)))
                .andExpect(status().isBadRequest()).andReturn());
        assertThat(error.get("legacyProgram").asText()).isEqualTo("COACTUPC");
        assertThat(error.get("fieldErrors").findValuesAsText("message")).contains(
                "State: is not a valid state code",
                "FICO Score: should be between 300 and 850",
                "SSN: First 3 chars: should not be 000, 666, or between 900 and 999");
    }

    @Test
    void updateRejectsZipNotValidForState() throws Exception {
        String token = userToken();
        ObjectNode request = editableAccount(token);
        ((ObjectNode) request.get("customer")).put("addrZip", "90210");
        mvc.perform(as(token, withJson(put("/api/v1/accounts/50"), request)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].message").value("Invalid zip code for state"));
    }
}
