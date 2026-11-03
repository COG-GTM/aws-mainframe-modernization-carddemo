package com.carddemo.auth.flow;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.loader.SampleDataLoader;
import com.carddemo.testsupport.flow.SampleDatabase;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import javax.sql.DataSource;
import org.junit.jupiter.api.BeforeAll;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.test.web.servlet.MockMvc;

/**
 * Online flow tests of auth-service: the sample USRSEC users (ADMIN001-005 type A, USER0001-005 type U,
 * password {@code PASSWORD}) loaded by the data-loader into the flow database, and real sign-on tokens.
 */
@SpringBootTest(properties = "spring.datasource.url=" + SampleDatabase.URL)
@AutoConfigureMockMvc
abstract class AuthFlowTestSupport {

    static final ObjectMapper JSON = new ObjectMapper();

    @Autowired
    MockMvc mockMvc;

    @BeforeAll
    static void loadSampleData(@Autowired DataSource dataSource) throws Exception {
        SampleDatabase.reload(dataSource, connection ->
                new SampleDataLoader(SampleDatabase.DATA_DIR, new BCryptPasswordEncoder()).load(connection, true));
    }

    /** COSGN00C with the given credentials; returns the bearer token. */
    String signOn(String userId, String password) throws Exception {
        String body = mockMvc.perform(post("/api/v1/auth/signon").contentType(MediaType.APPLICATION_JSON)
                        .content(JSON.writeValueAsString(java.util.Map.of("userId", userId, "password", password))))
                .andExpect(status().isOk())
                .andReturn().getResponse().getContentAsString();
        JsonNode response = JSON.readTree(body);
        return response.get("token").asText();
    }

    static String bearer(String token) {
        return "Bearer " + token;
    }
}
