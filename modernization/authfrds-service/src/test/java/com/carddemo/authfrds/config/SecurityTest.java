package com.carddemo.authfrds.config;

import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.httpBasic;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;

/** Full context, profile {@code test} only (no {@code testing}): auth rules + test hooks absent. No Db2 needed. */
@SpringBootTest(properties = "management.health.db.enabled=false")
@AutoConfigureMockMvc
@ActiveProfiles("test")
class SecurityTest {

    @Autowired
    MockMvc mvc;

    static final String KEY_PATH = "/api/v1/accounts/1/authorizations";

    @Test
    @DisplayName("BR-AF-040 no credentials -> 401 with a Basic challenge")
    void anonymous() throws Exception {
        mvc.perform(get("/api/v1/accounts")).andExpect(status().isUnauthorized())
                .andExpect(header().string("WWW-Authenticate", org.hamcrest.Matchers.startsWith("Basic")));
    }

    @Test
    @DisplayName("BR-AF-040 wrong password -> 401")
    void wrongPassword() throws Exception {
        mvc.perform(get("/api/v1/accounts").with(httpBasic("test-reader", "nope"))).andExpect(status().isUnauthorized());
    }

    @Test
    @DisplayName("BR-AF-040 reader GET -> 200")
    void readerGet() throws Exception {
        mvc.perform(get("/api/v1/accounts").with(httpBasic("test-reader", "test-reader-pw"))).andExpect(status().isOk());
    }

    @Test
    @DisplayName("BR-AF-040 reader write (PF5) -> 403")
    void readerWrite() throws Exception {
        mvc.perform(post(KEY_PATH + "/76699-998747444/fraud").with(httpBasic("test-reader", "test-reader-pw")))
                .andExpect(status().isForbidden());
        mvc.perform(post("/api/v1/fraud-records").with(httpBasic("test-reader", "test-reader-pw"))
                .contentType("application/json").content("{}")).andExpect(status().isForbidden());
    }

    @Test
    @DisplayName("BR-AF-040 maintainer write passes auth (400 validation, not 401/403)")
    void maintainerWrite() throws Exception {
        mvc.perform(post("/api/v1/fraud-records").with(httpBasic("test-maintainer", "test-maintainer-pw"))
                .contentType("application/json").content("{}")).andExpect(status().isBadRequest());
    }

    @Test
    @DisplayName("BR-AF-040 maintainer GET -> 200")
    void maintainerGet() throws Exception {
        mvc.perform(get(KEY_PATH).with(httpBasic("test-maintainer", "test-maintainer-pw"))).andExpect(status().isOk());
    }

    @Test
    @DisplayName("BR-AF-040 /actuator/health, Swagger UI and api-docs are public; api-docs declares basicAuth")
    void publicEndpoints() throws Exception {
        mvc.perform(get("/actuator/health")).andExpect(status().isOk()).andExpect(jsonPath("$.status").value("UP"));
        mvc.perform(get("/swagger-ui/index.html")).andExpect(status().isOk());
        mvc.perform(get("/v3/api-docs")).andExpect(status().isOk())
                .andExpect(jsonPath("$.components.securitySchemes.basicAuth.scheme").value("basic"))
                .andExpect(jsonPath("$.security[0].basicAuth").exists());
    }

    @Test
    @DisplayName("BR-AF-041 IMS stub test hooks are not registered without the 'testing' profile -> 404")
    void testHooksAbsent() throws Exception {
        mvc.perform(post("/api/v1/ims-stub/reset").with(httpBasic("test-maintainer", "test-maintainer-pw")))
                .andExpect(status().isNotFound());
        mvc.perform(post("/api/v1/ims-stub/fail-next-repl").with(httpBasic("test-maintainer", "test-maintainer-pw")))
                .andExpect(status().isNotFound());
    }
}
