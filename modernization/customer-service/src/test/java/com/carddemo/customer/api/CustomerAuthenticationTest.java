package com.carddemo.customer.api;

import com.carddemo.testsupport.AbstractAuthenticationTest;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest
@AutoConfigureMockMvc
class CustomerAuthenticationTest extends AbstractAuthenticationTest {

    @Override
    protected String protectedEndpoint() {
        return "/api/v1/customers?size=1";
    }
}
