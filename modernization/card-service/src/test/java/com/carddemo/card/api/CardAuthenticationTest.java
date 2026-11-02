package com.carddemo.card.api;

import com.carddemo.testsupport.AbstractAuthenticationTest;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest
@AutoConfigureMockMvc
class CardAuthenticationTest extends AbstractAuthenticationTest {

    @Override
    protected String protectedEndpoint() {
        return "/api/v1/cards?size=1";
    }
}
