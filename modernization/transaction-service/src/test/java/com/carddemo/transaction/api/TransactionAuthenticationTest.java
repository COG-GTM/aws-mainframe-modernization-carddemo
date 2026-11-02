package com.carddemo.transaction.api;

import com.carddemo.testsupport.AbstractAuthenticationTest;
import com.carddemo.transaction.support.StubGateways;
import org.springframework.context.annotation.Import;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest
@AutoConfigureMockMvc
@Import(StubGateways.class)
class TransactionAuthenticationTest extends AbstractAuthenticationTest {

    @Override
    protected String protectedEndpoint() {
        return "/api/v1/transactions?size=1";
    }
}
