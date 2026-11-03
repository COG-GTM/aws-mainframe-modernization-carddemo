package com.carddemo.batch.flow;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.common.security.TokenService;
import com.carddemo.loader.SampleDataLoader;
import com.carddemo.testsupport.flow.CobolFlow;
import com.carddemo.testsupport.flow.CobolMessages;
import com.carddemo.testsupport.flow.SampleDatabase;
import com.carddemo.testsupport.flow.ScreenKey;
import com.carddemo.testsupport.flow.ServiceStandIn;
import com.carddemo.testsupport.flow.ServiceStandIn.Response;
import com.fasterxml.jackson.databind.JsonNode;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.time.LocalDate;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import javax.sql.DataSource;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;

/**
 * CR00 transaction report submission (CORPT00C: report type, six custom date fields, SUBMIT-JOB-TO-INTRDR
 * as an in-process TRANREPT job, 202 + poll + content). card-service and transaction-service are replaced at
 * the HTTP boundary by a {@link ServiceStandIn} serving the CARDXREF, TRANTYPE, TRANCATG and DALYTRAN
 * sample rows from the flow database, so the job's real REST readers run.
 *
 * <p>The stand-in serves DALYTRAN as TRANSACT. DALYTRAN's TRAN-PROC-TS is blank in the sample file;
 * CBTRN02C stamps it with the POSTTRAN run time, so the stand-in stamps every row with
 * {@link #POSTTRAN_RUN} (any run date would do: the report filters on that date only).
 */
@SpringBootTest(properties = {"spring.datasource.url=" + SampleDatabase.URL,
        "carddemo.batch.output-dir=target/flow-batch-out"})
@AutoConfigureMockMvc
class ReportSubmissionFlowTest {

    static final ServiceStandIn SERVICES = ServiceStandIn.start();
    static final java.time.LocalDateTime POSTTRAN_RUN = java.time.LocalDateTime.of(2022, 7, 18, 23, 59, 0);
    static JdbcTemplate jdbc;

    @Autowired
    MockMvc mockMvc;

    @Autowired
    TokenService tokens;

    @DynamicPropertySource
    static void serviceUrls(DynamicPropertyRegistry registry) {
        registry.add("carddemo.services.card-url", SERVICES::url);
        registry.add("carddemo.services.transaction-url", SERVICES::url);
    }

    @BeforeAll
    static void loadSampleData(@Autowired DataSource dataSource) throws Exception {
        SampleDatabase.reload(dataSource, connection ->
                new SampleDataLoader(SampleDatabase.DATA_DIR, new BCryptPasswordEncoder()).load(connection, true));
        jdbc = new JdbcTemplate(dataSource);
        SERVICES.route("GET", "/api/v1/cards/(\\d+)/xref", r -> {
            List<Map<String, Object>> rows = jdbc.query(
                    "SELECT card_num, cust_id, acct_id FROM card.card_xref WHERE card_num = ?",
                    (rs, n) -> Map.of("cardNumber", rs.getString(1), "customerId", rs.getString(2),
                            "accountId", rs.getString(3)), r.group(1));
            return rows.isEmpty() ? Response.notFound() : Response.ok(rows.get(0));
        });
        SERVICES.route("GET", "/api/v1/transaction-types", r -> Response.ok(ServiceStandIn.page(r, jdbc.query(
                "SELECT type_cd, type_desc FROM transaction.transaction_types ORDER BY type_cd",
                (rs, n) -> Map.of("typeCode", rs.getString(1), "description", rs.getString(2))))));
        SERVICES.route("GET", "/api/v1/transaction-categories", r -> Response.ok(ServiceStandIn.page(r, jdbc.query(
                "SELECT type_cd, cat_cd, cat_type_desc FROM transaction.transaction_categories ORDER BY type_cd, cat_cd",
                (rs, n) -> Map.of("typeCode", rs.getString(1), "categoryCode", rs.getInt(2),
                        "description", rs.getString(3))))));
        SERVICES.route("GET", "/api/v1/transactions", r -> Response.ok(ServiceStandIn.page(r, jdbc.query(
                "SELECT * FROM transaction.daily_transactions ORDER BY tran_id", (rs, n) -> {
                    Map<String, Object> t = new LinkedHashMap<>();
                    t.put("transactionId", rs.getString("tran_id"));
                    t.put("cardNumber", rs.getString("card_num"));
                    t.put("typeCode", rs.getString("type_cd"));
                    t.put("categoryCode", rs.getInt("cat_cd"));
                    t.put("source", rs.getString("source"));
                    t.put("description", rs.getString("description"));
                    t.put("amount", rs.getBigDecimal("amount"));
                    t.put("merchantId", rs.getString("merchant_id"));
                    t.put("merchantName", rs.getString("merchant_name"));
                    t.put("merchantCity", rs.getString("merchant_city"));
                    t.put("merchantZip", rs.getString("merchant_zip"));
                    t.put("originTimestamp", rs.getTimestamp("orig_ts").toLocalDateTime());
                    t.put("processTimestamp", POSTTRAN_RUN);
                    return t;
                }))));
    }

    private ResultActions submit(Map<String, String> body) throws Exception {
        return mockMvc.perform(post("/api/v1/reports/transactions")
                .header(HttpHeaders.AUTHORIZATION, "Bearer " + tokens.issue("USER0001", "U").token())
                .contentType(MediaType.APPLICATION_JSON).content(ServiceStandIn.JSON.writeValueAsString(body)));
    }

    private static Map<String, String> custom(LocalDate start, LocalDate end) {
        Map<String, String> b = new LinkedHashMap<>();
        b.put("reportType", "CUSTOM");
        b.put("startMonth", "%02d".formatted(start.getMonthValue()));
        b.put("startDay", "%02d".formatted(start.getDayOfMonth()));
        b.put("startYear", String.valueOf(start.getYear()));
        b.put("endMonth", "%02d".formatted(end.getMonthValue()));
        b.put("endDay", "%02d".formatted(end.getDayOfMonth()));
        b.put("endYear", String.valueOf(end.getYear()));
        return b;
    }

    private JsonNode poll(String location) throws Exception {
        for (int i = 0; i < 240; i++) {
            JsonNode report = ServiceStandIn.JSON.readTree(mockMvc.perform(get(location)
                            .header(HttpHeaders.AUTHORIZATION, "Bearer " + tokens.issue("USER0001", "U").token()))
                    .andExpect(status().isOk()).andReturn().getResponse().getContentAsString());
            if (!"QUEUED".equals(report.get("status").asText()) && !"RUNNING".equals(report.get("status").asText())) {
                return report;
            }
            Thread.sleep(250);
        }
        throw new AssertionError("report still running after 60 s: " + location);
    }

    /** A valid custom range covering the POSTTRAN run date: 202, poll to COMPLETED, content. */
    @Test
    @CobolFlow(program = "CORPT00C", paragraph = "SUBMIT-JOB-TO-INTRDR", key = ScreenKey.ENTER)
    void CORPT00C_SUBMIT_JOB_TO_INTRDR_validCustomRangeProducesTheReport() throws Exception {
        LocalDate first = POSTTRAN_RUN.toLocalDate().withDayOfMonth(1);
        LocalDate last = POSTTRAN_RUN.toLocalDate().withDayOfMonth(31);
        String location = submit(custom(first, last))
                .andExpect(status().isAccepted())
                .andExpect(header().exists(HttpHeaders.LOCATION))
                .andExpect(jsonPath("$.reportType").value("CUSTOM"))
                .andExpect(jsonPath("$.startDate").value(first.toString()))
                .andExpect(jsonPath("$.endDate").value(last.toString()))
                .andExpect(jsonPath("$.submittedBy").value("USER0001"))
                .andReturn().getResponse().getHeader(HttpHeaders.LOCATION);
        JsonNode report = poll(location);
        assertThat(report.get("status").asText()).isEqualTo("COMPLETED");
        assertThat(report.get("returnCode").asInt()).isZero();
        assertThat(report.get("message").asText()).isEqualTo("Custom report completed");
        String content = mockMvc.perform(get(location + "/content")
                        .header(HttpHeaders.AUTHORIZATION, "Bearer " + tokens.issue("USER0001", "U").token()))
                .andExpect(status().isOk()).andReturn().getResponse().getContentAsString(StandardCharsets.UTF_8);
        assertThat(content).contains("DALYREPT", "Daily Transaction Report", "Grand Total");
        String someCard = jdbc.queryForObject(
                "SELECT card_num FROM transaction.daily_transactions ORDER BY tran_id LIMIT 1", String.class);
        String someTran = jdbc.queryForObject(
                "SELECT tran_id FROM transaction.daily_transactions ORDER BY tran_id LIMIT 1", String.class);
        assertThat(content).contains(someTran);
        assertThat(SERVICES.calls()).extracting(ServiceStandIn.Call::request)
                .contains("GET /api/v1/transactions", "GET /api/v1/cards/" + someCard + "/xref");
    }

    @Test
    @CobolFlow(program = "CORPT00C", paragraph = "PROCESS-ENTER-KEY", key = ScreenKey.ENTER)
    void CORPT00C_PROCESS_ENTER_KEY_invalidDateRangesAreRefusedWithTheCobolMessage() throws Exception {
        int queued = jdbc.queryForObject("SELECT count(*) FROM transaction_report_request", Integer.class);
        String[][] cases = {
                {"startMonth", "", "Start Date - Month can NOT be empty..."},
                {"startDay", "", "Start Date - Day can NOT be empty..."},
                {"startYear", "", "Start Date - Year can NOT be empty..."},
                {"endMonth", "", "End Date - Month can NOT be empty..."},
                {"endDay", "", "End Date - Day can NOT be empty..."},
                {"endYear", "", "End Date - Year can NOT be empty..."},
                {"startMonth", "13", "Start Date - Not a valid Month..."},
                {"startDay", "32", "Start Date - Not a valid Day..."},
                {"endMonth", "13", "End Date - Not a valid Month..."},
                {"endDay", "32", "End Date - Not a valid Day..."}};
        for (String[] c : cases) {
            Map<String, String> body = custom(LocalDate.of(2022, 1, 1), LocalDate.of(2022, 12, 31));
            body.put(c[0], c[1]);
            submit(body).andExpect(status().isBadRequest())
                    .andExpect(jsonPath("$.fieldErrors[0].field").value(c[0]))
                    .andExpect(jsonPath("$.fieldErrors[0].message").value(c[2]));
        }
        Map<String, String> february30 = custom(LocalDate.of(2022, 2, 1), LocalDate.of(2022, 12, 31));
        february30.put("startDay", "30");
        submit(february30).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].message").value("Start Date - Not a valid date..."));
        Map<String, String> june31 = custom(LocalDate.of(2022, 1, 1), LocalDate.of(2022, 6, 1));
        june31.put("endDay", "31");
        submit(june31).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].message").value("End Date - Not a valid date..."));
        submit(Map.of()).andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].message").value("Select a report type to print report..."));
        assertThat(jdbc.queryForObject("SELECT count(*) FROM transaction_report_request", Integer.class))
                .as("a refused submission queues nothing").isEqualTo(queued);
    }

    /**
     * Every CORPT00C message: asserted above, or not produced by the API (the CORPT00C MOVEs span two
     * source lines, so they are listed here and checked against the COBOL text).
     */
    @Test
    void CORPT00C_messagesAreAssertedOrExempt() throws Exception {
        String cobol = String.join("\n", Files.readAllLines(CobolMessages.COBOL_DIR.resolve("CORPT00C.cbl"),
                StandardCharsets.ISO_8859_1));
        String test = Files.readString(CobolMessages.source(ReportSubmissionFlowTest.class));
        List<String> asserted = List.of("Select a report type to print report...",
                "Start Date - Month can NOT be empty...", "Start Date - Day can NOT be empty...",
                "Start Date - Year can NOT be empty...", "End Date - Month can NOT be empty...",
                "End Date - Day can NOT be empty...", "End Date - Year can NOT be empty...",
                "Start Date - Not a valid Month...", "Start Date - Not a valid Day...",
                "End Date - Not a valid Month...", "End Date - Not a valid Day...",
                "Start Date - Not a valid date...", "End Date - Not a valid date...");
        Map<String, String> exempt = Map.of(
                "Start Date - Not a valid Year...", "IS NOT NUMERIC after FUNCTION NUMVAL-C cannot be true (ReportDates)",
                "End Date - Not a valid Year...", "IS NOT NUMERIC after FUNCTION NUMVAL-C cannot be true (ReportDates)",
                "Please confirm to print the ", "Y/N confirmation step; the POST is the confirmation (api-style.md §1)",
                "\" is not a valid value to confirm...", "Y/N confirmation step");
        assertThat(asserted).allSatisfy(m -> assertThat(cobol).contains("'" + m + "'"));
        assertThat(asserted).allSatisfy(m -> assertThat(test).contains('"' + m + '"'));
        assertThat(exempt.keySet()).allSatisfy(m -> assertThat(cobol).contains(m));
    }
}
