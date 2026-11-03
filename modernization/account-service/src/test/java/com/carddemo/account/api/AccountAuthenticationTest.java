package com.carddemo.account.api;

import com.carddemo.testsupport.AbstractAuthenticationTest;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest
@AutoConfigureMockMvc
class AccountAuthenticationTest extends AbstractAuthenticationTest {

    @Override
    protected String protectedEndpoint() {
        return "/api/v1/accounts?size=1";
    }
}
