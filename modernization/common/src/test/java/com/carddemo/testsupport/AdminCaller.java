package com.carddemo.testsupport;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;

import com.carddemo.common.security.TokenService;
import org.springframework.boot.test.autoconfigure.web.servlet.MockMvcBuilderCustomizer;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.http.HttpHeaders;

/**
 * {@code @Import} into a MockMvc test to send an admin ({@code A}) bearer token on every request, so
 * business-logic tests need not sign on. Kept outside {@code com.carddemo.common} so the services'
 * component scan never picks it up.
 */
@TestConfiguration
public class AdminCaller {

    public static final String USER_ID = "TESTADM1";

    @Bean
    MockMvcBuilderCustomizer adminBearerToken(TokenService tokens) {
        String header = "Bearer " + tokens.issue(USER_ID, "A").token();
        return builder -> builder.defaultRequest(get("/").header(HttpHeaders.AUTHORIZATION, header));
    }
}
