package com.carddemo;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Map;
import org.junit.jupiter.api.Test;

class TransactionTypeIntegrationTest extends IntegrationTestBase {

    @Test
    void listAndDetailAreReadableByUsers() throws Exception {
        String token = userToken();
        mvc.perform(as(token, get("/api/v1/transaction-types")))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.items.length()").value(7))
                .andExpect(jsonPath("$.items[0].typeCd").value("01"))
                .andExpect(jsonPath("$.items[0].description").value("Purchase"));
        mvc.perform(as(token, get("/api/v1/transaction-types").param("description", "pay")))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.items[0].typeCd").value("02"));
        mvc.perform(as(token, get("/api/v1/transaction-types/2")))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.typeCd").value("02"));
        mvc.perform(as(token, get("/api/v1/transaction-types/01/categories")))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.length()").value(5))
                .andExpect(jsonPath("$[0].description").value("Regular Sales Draft"));
    }

    @Test
    void detailValidatesAndReportsMissingType() throws Exception {
        String token = userToken();
        mvc.perform(as(token, get("/api/v1/transaction-types/AB")))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Tran Type code must be numeric."));
        mvc.perform(as(token, get("/api/v1/transaction-types/99")))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("No record found for this key in database"));
    }

    @Test
    void writesAreAdminOnly() throws Exception {
        mvc.perform(as(userToken(), withJson(post("/api/v1/transaction-types"),
                Map.of("typeCd", "08", "description", "Fee"))))
                .andExpect(status().isForbidden());
    }

    @Test
    void adminMaintainsTransactionTypes() throws Exception {
        String admin = adminToken();
        mvc.perform(as(admin, withJson(post("/api/v1/transaction-types"),
                Map.of("typeCd", "8", "description", "Annual Fee"))))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.typeCd").value("08"));
        mvc.perform(as(admin, withJson(post("/api/v1/transaction-types"),
                Map.of("typeCd", "08", "description", "Annual Fee"))))
                .andExpect(status().isConflict())
                .andExpect(jsonPath("$.errorCode").value("DUPLICATE"));
        mvc.perform(as(admin, withJson(put("/api/v1/transaction-types/08"),
                Map.of("description", "Yearly Fee", "version", 0))))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.description").value("Yearly Fee"))
                .andExpect(jsonPath("$.version").value(1));
        mvc.perform(as(admin, withJson(put("/api/v1/transaction-types/08"),
                Map.of("description", "Other Fee", "version", 0))))
                .andExpect(status().isConflict())
                .andExpect(jsonPath("$.errorCode").value("CONCURRENT_UPDATE"));
        mvc.perform(as(admin, delete("/api/v1/transaction-types/08").param("version", "1")))
                .andExpect(status().isNoContent());
    }

    @Test
    void deleteOfTypeWithCategoriesIsIntegrityViolation() throws Exception {
        mvc.perform(as(adminToken(), delete("/api/v1/transaction-types/05")))
                .andExpect(status().isConflict())
                .andExpect(jsonPath("$.errorCode").value("INTEGRITY_VIOLATION"));
    }

    @Test
    void createValidatesDescription() throws Exception {
        mvc.perform(as(adminToken(), withJson(post("/api/v1/transaction-types"),
                Map.of("typeCd", "09", "description", "Bad*Desc"))))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Transaction Desc can have numbers or alphabets only."));
    }
}
