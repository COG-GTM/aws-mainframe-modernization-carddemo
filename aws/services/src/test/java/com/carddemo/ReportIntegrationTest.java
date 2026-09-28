package com.carddemo;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.time.LocalDate;
import java.time.YearMonth;
import java.util.HashMap;
import java.util.Map;
import org.junit.jupiter.api.Test;

class ReportIntegrationTest extends IntegrationTestBase {

    @Test
    void monthlyReportUsesCurrentMonth() throws Exception {
        YearMonth month = YearMonth.now();
        mvc.perform(as(userToken(), withJson(post("/api/v1/reports/transactions"), Map.of("reportType", "MONTHLY"))))
                .andExpect(status().isAccepted())
                .andExpect(jsonPath("$.requestId").isNotEmpty())
                .andExpect(jsonPath("$.startDate").value(month.atDay(1).toString()))
                .andExpect(jsonPath("$.endDate").value(month.atEndOfMonth().toString()))
                .andExpect(jsonPath("$.message").value("Monthly report submitted for printing ..."));
    }

    @Test
    void yearlyReportUsesCurrentYear() throws Exception {
        int year = LocalDate.now().getYear();
        mvc.perform(as(userToken(), withJson(post("/api/v1/reports/transactions"), Map.of("reportType", "yearly"))))
                .andExpect(status().isAccepted())
                .andExpect(jsonPath("$.startDate").value(year + "-01-01"))
                .andExpect(jsonPath("$.endDate").value(year + "-12-31"));
    }

    @Test
    void customReportValidatesDates() throws Exception {
        String token = userToken();
        Map<String, Object> body = new HashMap<>();
        body.put("reportType", "CUSTOM");
        body.put("startDate", "2022-01-01");
        body.put("endDate", "2022-02-30");
        mvc.perform(as(token, withJson(post("/api/v1/reports/transactions"), body)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("End Date - Not a valid date..."))
                .andExpect(jsonPath("$.legacyProgram").value("CORPT00C"));
        body.put("endDate", "2022-12-31");
        mvc.perform(as(token, withJson(post("/api/v1/reports/transactions"), body)))
                .andExpect(status().isAccepted())
                .andExpect(jsonPath("$.message").value("Custom report submitted for printing ..."));
    }

    @Test
    void reportTypeIsRequired() throws Exception {
        mvc.perform(as(userToken(), withJson(post("/api/v1/reports/transactions"), Map.of("reportType", ""))))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.message").value("Select a report type to print report..."));
    }

    @Test
    void statusOfSubmittedRequest() throws Exception {
        String token = userToken();
        String requestId = body(mvc.perform(as(token, withJson(post("/api/v1/reports/transactions"),
                Map.of("reportType", "MONTHLY")))).andReturn()).get("requestId").asText();
        mvc.perform(as(token, get("/api/v1/reports/transactions/" + requestId)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.requestId").value(requestId))
                .andExpect(jsonPath("$.status").value("SUBMITTED"));
        mvc.perform(as(token, get("/api/v1/reports/transactions/not-a-uuid")))
                .andExpect(status().isBadRequest());
    }
}
