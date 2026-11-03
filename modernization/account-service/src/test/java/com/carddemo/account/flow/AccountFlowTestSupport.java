package com.carddemo.account.flow;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.common.security.TokenService;
import com.carddemo.loader.SampleDataLoader;
import com.carddemo.testsupport.flow.SampleDatabase;
import com.carddemo.testsupport.flow.ServiceStandIn;
import com.carddemo.testsupport.flow.ServiceStandIn.Response;
import com.fasterxml.jackson.databind.JsonNode;
import java.math.BigDecimal;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import javax.sql.DataSource;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpHeaders;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;

/**
 * Online flow tests of account-service on the loaded sample data. card-service and customer-service are
 * replaced at the HTTP boundary by a {@link ServiceStandIn} that answers their endpoints
 * ({@code GET /api/v1/card-xrefs/by-account/{id}}, {@code GET/PUT /api/v1/customers/{id}}) from the
 * {@code card} and {@code customer} schemas of the same flow database, so account-service's real REST
 * clients, JSON and token forwarding run.
 */
@SpringBootTest(properties = "spring.datasource.url=" + SampleDatabase.URL)
@AutoConfigureMockMvc
abstract class AccountFlowTestSupport {

    static final ServiceStandIn SERVICES = ServiceStandIn.start();
    static JdbcTemplate jdbc;

    @Autowired
    MockMvc mockMvc;

    @Autowired
    TokenService tokens;

    @DynamicPropertySource
    static void serviceUrls(DynamicPropertyRegistry registry) {
        registry.add("carddemo.services.card-url", SERVICES::url);
        registry.add("carddemo.services.customer-url", SERVICES::url);
    }

    @BeforeAll
    static void loadSampleData(@Autowired DataSource dataSource) throws Exception {
        SampleDatabase.reload(dataSource, connection ->
                new SampleDataLoader(SampleDatabase.DATA_DIR, new BCryptPasswordEncoder()).load(connection, true));
        jdbc = new JdbcTemplate(dataSource);
        SERVICES.route("GET", "/api/v1/card-xrefs/by-account/(\\d+)", r -> first(jdbc.query(
                "SELECT card_num, cust_id, acct_id FROM card.card_xref WHERE acct_id = ? ORDER BY card_num",
                (rs, n) -> Map.of("cardNumber", rs.getString(1), "customerId", rs.getLong(2), "accountId", rs.getLong(3)),
                Long.parseLong(r.group(1)))));
        SERVICES.route("GET", "/api/v1/customers/(\\d+)", r -> first(customer(Long.parseLong(r.group(1)))));
        SERVICES.route("PUT", "/api/v1/customers/(\\d+)", r -> updateCustomer(Long.parseLong(r.group(1)),
                ServiceStandIn.JSON.readTree(r.body())));
    }

    @AfterAll
    static void forgetRoutes() {
        SERVICES.clearCalls();
    }

    JsonNode view(long accountId) throws Exception {
        return ServiceStandIn.JSON.readTree(mockMvc.perform(asUser(get("/api/v1/accounts/" + accountId + "/view")))
                .andExpect(status().isOk()).andReturn().getResponse().getContentAsString());
    }

    ResultActions update(String accountId, Map<String, Object> body) throws Exception {
        return mockMvc.perform(asUser(put("/api/v1/accounts/" + accountId).contentType(MediaType.APPLICATION_JSON)
                .content(ServiceStandIn.JSON.writeValueAsString(body))));
    }

    /** The CAUP screen after ENTER: every input field as COACTUPC shows it, plus both versions. */
    static Map<String, Object> screen(JsonNode v) {
        Map<String, Object> b = new LinkedHashMap<>();
        b.put("accountVersion", v.get("accountVersion").asLong());
        b.put("customerVersion", v.get("customerVersion").asLong());
        b.put("activeStatus", text(v, "activeStatus"));
        b.put("creditLimit", v.get("creditLimit").decimalValue().toPlainString());
        b.put("cashCreditLimit", v.get("cashCreditLimit").decimalValue().toPlainString());
        b.put("currentBalance", v.get("currentBalance").decimalValue().toPlainString());
        b.put("currentCycleCredit", v.get("currentCycleCredit").decimalValue().toPlainString());
        b.put("currentCycleDebit", v.get("currentCycleDebit").decimalValue().toPlainString());
        b.put("openDate", text(v, "openDate"));
        b.put("expirationDate", text(v, "expirationDate"));
        b.put("reissueDate", text(v, "reissueDate"));
        b.put("groupId", text(v, "groupId"));
        b.put("ssn", "");
        b.put("dateOfBirth", text(v, "dateOfBirth"));
        b.put("ficoScore", text(v, "ficoScore"));
        b.put("firstName", text(v, "firstName"));
        b.put("middleName", text(v, "middleName"));
        b.put("lastName", text(v, "lastName"));
        b.put("addressLine1", text(v, "addressLine1"));
        b.put("addressLine2", text(v, "addressLine2"));
        b.put("city", text(v, "city"));
        b.put("stateCode", text(v, "stateCode"));
        b.put("zip", text(v, "zip"));
        b.put("phoneNumber1", text(v, "phoneNumber1"));
        b.put("phoneNumber2", text(v, "phoneNumber2"));
        b.put("governmentIssuedId", text(v, "governmentIssuedId"));
        b.put("eftAccountId", text(v, "eftAccountId"));
        b.put("primaryCardHolder", text(v, "primaryCardHolder"));
        return b;
    }

    /**
     * Customer 1 as stored fails three COACTUPC edits (NC with ZIP 12546, area code 373, FICO 274); these
     * corrections make the screen pass every edit, so one changed field at a time can be tested.
     */
    static Map<String, Object> validScreen(JsonNode v) {
        Map<String, Object> b = screen(v);
        b.put("zip", "27514");
        b.put("phoneNumber2", "(212)693-8684");
        b.put("ficoScore", "700");
        return b;
    }

    /** A text field of the view as the screen shows it: blank for null. */
    static String text(JsonNode v, String field) {
        JsonNode node = v.path(field);
        return node.isNull() || node.isMissingNode() ? "" : node.asText();
    }

    MockHttpServletRequestBuilder asUser(MockHttpServletRequestBuilder request) {
        return request.header(HttpHeaders.AUTHORIZATION, "Bearer " + tokens.issue("USER0001", "U").token());
    }

    private static Response first(List<?> rows) {
        return rows.isEmpty() ? Response.notFound() : Response.ok(rows.get(0));
    }

    /** customer-service's CustomerResponse of a CUSTDAT row. */
    static List<Map<String, Object>> customer(long id) {
        return jdbc.query("SELECT * FROM customer.customers WHERE cust_id = ?", (rs, n) -> customerJson(rs), id);
    }

    private static Map<String, Object> customerJson(ResultSet rs) throws SQLException {
        Map<String, Object> c = new LinkedHashMap<>();
        c.put("customerId", rs.getLong("cust_id"));
        c.put("firstName", rs.getString("first_name"));
        c.put("middleName", rs.getString("middle_name"));
        c.put("lastName", rs.getString("last_name"));
        c.put("addressLine1", rs.getString("addr_line_1"));
        c.put("addressLine2", rs.getString("addr_line_2"));
        c.put("addressLine3", rs.getString("addr_line_3"));
        c.put("stateCode", rs.getString("addr_state_cd"));
        c.put("countryCode", rs.getString("addr_country_cd"));
        c.put("zip", rs.getString("addr_zip"));
        c.put("phone1", rs.getString("phone_num_1"));
        c.put("phone2", rs.getString("phone_num_2"));
        long ssn = rs.getLong("ssn");
        c.put("ssnLast4", rs.wasNull() ? null : "%09d".formatted(ssn).substring(5));
        c.put("governmentIssuedId", rs.getString("govt_issued_id"));
        c.put("dateOfBirth", rs.getDate("date_of_birth") == null ? null : rs.getDate("date_of_birth").toString());
        c.put("eftAccountId", rs.getString("eft_account_id"));
        c.put("primaryCardHolderIndicator", rs.getString("pri_card_holder_ind"));
        c.put("ficoScore", rs.getInt("fico_credit_score"));
        c.put("version", rs.getLong("version"));
        return c;
    }

    /** customer-service's PUT: version check (409), rewrite, version + 1. */
    private static Response updateCustomer(long id, JsonNode u) {
        List<Map<String, Object>> stored = customer(id);
        if (stored.isEmpty()) {
            return Response.notFound();
        }
        if (u.hasNonNull("version") && u.get("version").asLong() != (Long) stored.get(0).get("version")) {
            return new Response(409, Map.of("status", 409, "message", "Record changed by some one else. Please review"));
        }
        jdbc.update("""
                UPDATE customer.customers SET first_name = ?, middle_name = ?, last_name = ?, addr_line_1 = ?,
                    addr_line_2 = ?, addr_line_3 = ?, addr_state_cd = ?, addr_country_cd = ?, addr_zip = ?,
                    phone_num_1 = ?, phone_num_2 = ?, ssn = COALESCE(?, ssn), govt_issued_id = ?,
                    date_of_birth = CAST(? AS DATE), eft_account_id = ?, pri_card_holder_ind = ?,
                    fico_credit_score = ?, version = version + 1
                WHERE cust_id = ?""",
                jsonText(u, "firstName"), jsonText(u, "middleName"), jsonText(u, "lastName"), jsonText(u, "addressLine1"),
                jsonText(u, "addressLine2"), jsonText(u, "addressLine3"), jsonText(u, "stateCode"), jsonText(u, "countryCode"),
                jsonText(u, "zip"), jsonText(u, "phone1"), jsonText(u, "phone2"),
                u.hasNonNull("ssn") ? u.get("ssn").asLong() : null, jsonText(u, "governmentIssuedId"),
                jsonText(u, "dateOfBirth"), jsonText(u, "eftAccountId"), jsonText(u, "primaryCardHolderIndicator"),
                u.hasNonNull("ficoScore") ? u.get("ficoScore").asInt() : null, id);
        return Response.ok(customer(id).get(0));
    }

    private static String jsonText(JsonNode node, String field) {
        return node.hasNonNull(field) ? node.get(field).asText() : null;
    }
}
