package com.carddemo.transaction.flow;

import com.carddemo.common.security.TokenService;
import com.carddemo.loader.SampleDataLoader;
import com.carddemo.testsupport.flow.SampleDatabase;
import com.carddemo.transaction.support.StubGateways;
import com.carddemo.testsupport.flow.ServiceStandIn;
import com.carddemo.testsupport.flow.ServiceStandIn.Response;
import com.fasterxml.jackson.databind.JsonNode;
import java.math.BigDecimal;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.sql.DataSource;
import org.junit.jupiter.api.BeforeAll;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.beans.factory.support.BeanDefinitionRegistryPostProcessor;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Import;
import org.springframework.http.HttpHeaders;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;

/**
 * Online flow tests of transaction-service on the loaded sample data. account-service and card-service
 * are replaced at the HTTP boundary by a {@link ServiceStandIn} answering from the {@code account} and
 * {@code card} schemas of the flow database: {@code GET /api/v1/accounts/{id}},
 * {@code POST /api/v1/accounts/{id}/postings} (the COBIL00C {@code BILL_PAYMENT} posting: balance only),
 * {@code GET /api/v1/cards/{card}/xref} and {@code GET /api/v1/card-xrefs/by-account/{id}}.
 *
 * <p>TRANSACT is empty after a load (POSTTRAN fills it). The flow tests start from TRANSACT as POSTTRAN
 * would leave it for the sample DALYTRAN: the 300 daily transactions copied field for field.
 * <b>Hypothesis:</b> the copy is what CBTRN02C writes for an accepted record apart from TRAN-PROC-TS
 * (which it sets to the run time); the 38 records POSTTRAN rejects are kept too, since the list screens
 * do not depend on the reject rule.
 */
@SpringBootTest(properties = "spring.datasource.url=" + SampleDatabase.URL)
@AutoConfigureMockMvc
@Import(TransactionFlowTestSupport.RealGateways.class)
abstract class TransactionFlowTestSupport {

    /**
     * TransactionServiceApplication's own {@code @ComponentScan} has no TypeExcludeFilter, so the
     * top-level {@code @TestConfiguration} StubGateways is scanned into every test context and its
     * {@code @Primary} in-memory gateways win. The flow tests drop those definitions so the real
     * RestAccountGateway/RestCardGateway call the stand-in over HTTP. Deliberately not annotated: that
     * same scan would otherwise pick this class up and drop StubGateways from every other test context.
     */
    static class RealGateways {
        @Bean
        static BeanDefinitionRegistryPostProcessor dropStubGateways() {
            return registry -> {
                Set<String> stubConfigs = new HashSet<>();
                for (String name : registry.getBeanDefinitionNames()) {
                    String type = registry.getBeanDefinition(name).getBeanClassName();
                    if (type != null && type.startsWith(StubGateways.class.getName())) {
                        stubConfigs.add(name);
                    }
                }
                for (String name : registry.getBeanDefinitionNames()) {
                    if (stubConfigs.contains(registry.getBeanDefinition(name).getFactoryBeanName())) {
                        registry.removeBeanDefinition(name);
                    }
                }
                stubConfigs.forEach(registry::removeBeanDefinition);
            };
        }
    }

    static final ServiceStandIn SERVICES = ServiceStandIn.start();
    static JdbcTemplate jdbc;

    @Autowired
    MockMvc mockMvc;

    @Autowired
    TokenService tokens;

    @DynamicPropertySource
    static void serviceUrls(DynamicPropertyRegistry registry) {
        registry.add("carddemo.services.account-url", SERVICES::url);
        registry.add("carddemo.services.card-url", SERVICES::url);
    }

    @BeforeAll
    static void loadSampleData(@Autowired DataSource dataSource) throws Exception {
        SampleDatabase.reload(dataSource, connection ->
                new SampleDataLoader(SampleDatabase.DATA_DIR, new BCryptPasswordEncoder()).load(connection, true));
        jdbc = new JdbcTemplate(dataSource);
        jdbc.update("""
                INSERT INTO transaction.transactions (tran_id, type_cd, cat_cd, source, description, amount,
                    merchant_id, merchant_name, merchant_city, merchant_zip, card_num, orig_ts, proc_ts)
                SELECT tran_id, type_cd, cat_cd, source, description, amount, merchant_id, merchant_name,
                    merchant_city, merchant_zip, card_num, orig_ts, proc_ts
                FROM transaction.daily_transactions""");
        SERVICES.route("GET", "/api/v1/accounts/(\\d+)", r -> first(jdbc.query(
                "SELECT acct_id, active_status, curr_bal, group_id FROM account.accounts WHERE acct_id = ?",
                (rs, n) -> {
                    Map<String, Object> a = new LinkedHashMap<>();
                    a.put("accountId", rs.getLong(1));
                    a.put("activeStatus", rs.getString(2));
                    a.put("currentBalance", rs.getBigDecimal(3));
                    a.put("groupId", rs.getString(4));
                    return a;
                }, Long.parseLong(r.group(1)))));
        SERVICES.route("POST", "/api/v1/accounts/(\\d+)/postings", r -> post(Long.parseLong(r.group(1)),
                ServiceStandIn.JSON.readTree(r.body())));
        SERVICES.route("GET", "/api/v1/cards/(\\d+)/xref", r -> first(xrefs("card_num = ?", r.group(1))));
        SERVICES.route("GET", "/api/v1/card-xrefs/by-account/(\\d+)", r -> first(xrefs("acct_id = ?",
                Long.parseLong(r.group(1)))));
    }

    MockHttpServletRequestBuilder asUser(MockHttpServletRequestBuilder request) {
        return request.header(HttpHeaders.AUTHORIZATION, "Bearer " + tokens.issue("USER0001", "U").token());
    }

    static BigDecimal balance(long accountId) {
        return jdbc.queryForObject("SELECT curr_bal FROM account.accounts WHERE acct_id = ?", BigDecimal.class, accountId);
    }

    private static List<Map<String, Object>> xrefs(String where, Object key) {
        return jdbc.query("SELECT card_num, cust_id, acct_id FROM card.card_xref WHERE " + where + " ORDER BY card_num",
                (rs, n) -> Map.of("cardNumber", rs.getString(1), "customerId", rs.getLong(2), "accountId", rs.getLong(3)),
                key);
    }

    /** account-service's posting endpoint for the COBIL00C type: ACCT-CURR-BAL + amount, nothing else. */
    private static Response post(long accountId, JsonNode body) {
        if (!"BILL_PAYMENT".equals(body.path("type").asText())) {
            return new Response(400, Map.of("status", 400, "message", "flow stand-in posts bill payments only"));
        }
        int updated = jdbc.update("UPDATE account.accounts SET curr_bal = curr_bal + ? WHERE acct_id = ?",
                body.get("amount").decimalValue(), accountId);
        if (updated == 0) {
            return Response.notFound();
        }
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("posted", true);
        result.put("reasonCode", null);
        result.put("reasonDescription", null);
        result.put("currentBalance", balance(accountId));
        result.put("currentCycleCredit", BigDecimal.ZERO);
        result.put("currentCycleDebit", BigDecimal.ZERO);
        return Response.ok(result);
    }

    private static Response first(List<?> rows) {
        return rows.isEmpty() ? Response.notFound() : Response.ok(rows.get(0));
    }
}
