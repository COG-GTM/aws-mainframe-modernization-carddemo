package com.carddemo.testsupport;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.common.security.TokenService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.test.web.servlet.MockMvc;

/**
 * The authentication contract every service must meet; subclasses are {@code @SpringBootTest
 * @AutoConfigureMockMvc} tests of one service and name one business endpoint.
 */
public abstract class AbstractAuthenticationTest {

    @Autowired
    protected MockMvc mockMvc;

    @Autowired
    protected TokenService tokens;

    /** A GET business endpoint that answers 200 for any signed-on user on an empty database. */
    protected abstract String protectedEndpoint();

    @Test
    void rejectsAMissingTokenWithApiError() throws Exception {
        mockMvc.perform(get(protectedEndpoint()))
                .andExpect(status().isUnauthorized())
                .andExpect(header().string(HttpHeaders.WWW_AUTHENTICATE, "Bearer"))
                .andExpect(jsonPath("$.status").value(401))
                .andExpect(jsonPath("$.error").value("Unauthorized"))
                .andExpect(jsonPath("$.message").value("Authentication required. Sign on first ..."))
                .andExpect(jsonPath("$.timestamp").exists())
                .andExpect(jsonPath("$.fieldErrors").isArray());
    }

    @Test
    void rejectsAnInvalidTokenWithApiError() throws Exception {
        String token = tokens.issue("ADMIN001", "A").token();
        int signature = token.lastIndexOf('.') + 1;
        char first = token.charAt(signature);
        String forged = token.substring(0, signature) + (first == 'A' ? 'B' : 'A') + token.substring(signature + 1);
        mockMvc.perform(get(protectedEndpoint()).header(HttpHeaders.AUTHORIZATION, "Bearer " + forged))
                .andExpect(status().isUnauthorized())
                .andExpect(jsonPath("$.message").value("Session token is invalid or expired. Sign on again ..."));
        mockMvc.perform(get(protectedEndpoint()).header(HttpHeaders.AUTHORIZATION, "Basic QURNSU4wMDE6UEFTU1dPUkQ="))
                .andExpect(status().isUnauthorized());
    }

    @Test
    void acceptsAUserToken() throws Exception {
        mockMvc.perform(get(protectedEndpoint())
                        .header(HttpHeaders.AUTHORIZATION, "Bearer " + tokens.issue("USER0001", "U").token()))
                .andExpect(status().isOk());
    }

    @Test
    void leavesHealthAndApiDocsOpen() throws Exception {
        mockMvc.perform(get("/actuator/health")).andExpect(status().isOk());
        mockMvc.perform(get("/v3/api-docs")).andExpect(status().isOk());
    }
}
