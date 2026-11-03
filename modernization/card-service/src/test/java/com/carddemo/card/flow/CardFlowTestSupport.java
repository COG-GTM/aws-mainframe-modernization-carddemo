package com.carddemo.card.flow;

import com.carddemo.common.security.TokenService;
import com.carddemo.loader.SampleDataLoader;
import com.carddemo.testsupport.flow.SampleDatabase;
import com.fasterxml.jackson.databind.ObjectMapper;
import javax.sql.DataSource;
import org.junit.jupiter.api.BeforeAll;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpHeaders;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;

/**
 * Online flow tests of card-service on the 50 sample cards (CARDDATA, CARDXREF) loaded by the
 * data-loader. Requests carry the token COSGN00C issues to the sample user USER0001 (type U): the card
 * screens are main-menu options, and COCRDLIC does not branch on user type.
 */
@SpringBootTest(properties = "spring.datasource.url=" + SampleDatabase.URL)
@AutoConfigureMockMvc
abstract class CardFlowTestSupport {

    static final ObjectMapper JSON = new ObjectMapper();

    @Autowired
    MockMvc mockMvc;

    @Autowired
    TokenService tokens;

    @BeforeAll
    static void loadSampleData(@Autowired DataSource dataSource) throws Exception {
        SampleDatabase.reload(dataSource, connection ->
                new SampleDataLoader(SampleDatabase.DATA_DIR, new BCryptPasswordEncoder()).load(connection, true));
    }

    MockHttpServletRequestBuilder asUser(MockHttpServletRequestBuilder request) {
        return request.header(HttpHeaders.AUTHORIZATION, "Bearer " + tokens.issue("USER0001", "U").token());
    }
}
