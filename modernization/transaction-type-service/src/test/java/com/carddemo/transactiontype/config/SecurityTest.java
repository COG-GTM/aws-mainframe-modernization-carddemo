package com.carddemo.transactiontype.config;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.httpBasic;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.transactiontype.domain.InMemoryTransactionTypeStore;
import com.carddemo.transactiontype.domain.TransactionTypeStore;
import java.util.Set;
import java.util.TreeSet;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Primary;
import org.springframework.http.MediaType;
import org.springframework.security.crypto.factory.PasswordEncoderFactories;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerMapping;

/** Full application context on the `test` profile (users from application-test.yml), in-memory store instead of Db2. */
@SpringBootTest
@AutoConfigureMockMvc
@ActiveProfiles("test")
class SecurityTest {

    private static final String API = "/api/v1/transaction-types";
    private static final String NEW_TYPE = "{\"code\":\"88\",\"description\":\"SECURITY TEST\"}";

    @TestConfiguration
    static class Store {
        @Bean
        @Primary
        TransactionTypeStore inMemoryStore() {
            return InMemoryTransactionTypeStore.seeded();
        }
    }

    @Autowired
    MockMvc mvc;

    @Autowired
    @Qualifier("requestMappingHandlerMapping")
    RequestMappingHandlerMapping mappings;

    @Test
    @DisplayName("no credentials -> 401 with a Basic challenge on reads and writes")
    void unauthenticated() throws Exception {
        mvc.perform(get(API)).andExpect(status().isUnauthorized())
                .andExpect(header().string("WWW-Authenticate", "Basic realm=\"transaction-type-service\""));
        mvc.perform(post(API).contentType(MediaType.APPLICATION_JSON).content(NEW_TYPE))
                .andExpect(status().isUnauthorized());
        mvc.perform(get("/api/v1/transaction-types/extract")).andExpect(status().isUnauthorized());
    }

    @Test
    @DisplayName("wrong password -> 401")
    void wrongPassword() throws Exception {
        mvc.perform(get(API).with(httpBasic("it-maintainer", "nope"))).andExpect(status().isUnauthorized());
    }

    @Test
    @DisplayName("reader: GET 200, POST/PUT/DELETE/batch 403")
    void readerIsReadOnly() throws Exception {
        var reader = httpBasic("it-reader", "it-reader-pw");
        mvc.perform(get(API).with(reader)).andExpect(status().isOk()).andExpect(jsonPath("$.items[0].code").value("01"));
        mvc.perform(get(API + "/01").with(reader)).andExpect(status().isOk());
        mvc.perform(get("/api/v1/transaction-types/extract").with(reader)).andExpect(status().isOk());
        mvc.perform(post(API).with(reader).contentType(MediaType.APPLICATION_JSON).content(NEW_TYPE))
                .andExpect(status().isForbidden());
        mvc.perform(put(API + "/01").with(reader).contentType(MediaType.APPLICATION_JSON)
                .content("{\"description\":\"X\"}")).andExpect(status().isForbidden());
        mvc.perform(delete(API + "/01?confirm=true").with(reader)).andExpect(status().isForbidden());
        mvc.perform(post(API + "/batch").with(reader).contentType(MediaType.TEXT_PLAIN).content("A88SECURITY"))
                .andExpect(status().isForbidden());
    }

    @Test
    @DisplayName("maintainer: GET 200, POST 201, PUT 200, DELETE 200")
    void maintainerCanWrite() throws Exception {
        var maintainer = httpBasic("it-maintainer", "it-maintainer-pw");
        mvc.perform(get(API).with(maintainer)).andExpect(status().isOk());
        mvc.perform(post(API).with(maintainer).contentType(MediaType.APPLICATION_JSON).content(NEW_TYPE))
                .andExpect(status().isCreated());
        mvc.perform(put(API + "/88").with(maintainer).contentType(MediaType.APPLICATION_JSON)
                .content("{\"description\":\"SECURITY TEST 2\"}")).andExpect(status().isOk());
        mvc.perform(delete(API + "/88?confirm=true").with(maintainer)).andExpect(status().isOk());
    }

    @Test
    @DisplayName("health, Swagger UI and the OpenAPI document are public; the document declares basicAuth")
    void publicEndpoints() throws Exception {
        mvc.perform(get("/actuator/health")).andExpect(result ->
                assertThat(result.getResponse().getStatus()).isNotIn(401, 403));
        mvc.perform(get("/swagger-ui/index.html")).andExpect(status().isOk());
        mvc.perform(get("/v3/api-docs")).andExpect(status().isOk())
                .andExpect(jsonPath("$.components.securitySchemes.basicAuth.scheme").value("basic"))
                .andExpect(jsonPath("$.security[0].basicAuth").isArray());
        mvc.perform(get("/actuator/info")).andExpect(status().isUnauthorized());
    }

    @Test
    @DisplayName("without the `testing` profile no test/debug endpoints exist: /api/v1/testing/** -> 404")
    void testingEndpointsAbsentByDefault() throws Exception {
        var maintainer = httpBasic("it-maintainer", "it-maintainer-pw");
        mvc.perform(post("/api/v1/testing/reset").with(maintainer)).andExpect(status().isNotFound());
        mvc.perform(get("/api/v1/testing/state").with(maintainer)).andExpect(status().isNotFound());

        Set<String> paths = new TreeSet<>();
        mappings.getHandlerMethods().keySet().forEach(info -> paths.addAll(info.getPatternValues()));
        assertThat(paths).allMatch(p -> p.startsWith("/api/v1/transaction-type")
                || p.startsWith("/v3/api-docs") || p.startsWith("/swagger-ui") || p.equals("/error"));
    }

    @Test
    @DisplayName("startup fails fast when any API credential is unset or the two users clash")
    void failsFastWithoutCredentials() {
        var config = new SecurityConfig();
        var encoder = PasswordEncoderFactories.createDelegatingPasswordEncoder();
        var ok = new ApiSecurityProperties.Credentials("m", "pw");
        assertThatThrownBy(() -> config.users(new ApiSecurityProperties(null, null), encoder))
                .isInstanceOf(IllegalStateException.class).hasMessageContaining("TT_API_USER");
        assertThatThrownBy(() -> config.users(
                new ApiSecurityProperties(ok, new ApiSecurityProperties.Credentials("r", "")), encoder))
                .hasMessageContaining("TT_API_READER_PASSWORD");
        assertThatThrownBy(() -> config.users(new ApiSecurityProperties(ok, ok), encoder))
                .hasMessageContaining("must be different");
    }
}
