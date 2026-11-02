package com.carddemo.common.security;

import static org.springframework.test.web.client.match.MockRestRequestMatchers.header;
import static org.springframework.test.web.client.match.MockRestRequestMatchers.headerDoesNotExist;
import static org.springframework.test.web.client.match.MockRestRequestMatchers.requestTo;
import static org.springframework.test.web.client.response.MockRestResponseCreators.withSuccess;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpHeaders;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.test.web.client.MockRestServiceServer;
import org.springframework.web.client.RestClient;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

class CallerTokenForwarderTest {

    private final RestClient.Builder builder = RestClient.builder().baseUrl("http://account-service")
            .requestInterceptor(new CallerTokenForwarder());
    private final MockRestServiceServer server = MockRestServiceServer.bindTo(builder).build();
    private final RestClient client = builder.build();

    @AfterEach
    void clear() {
        RequestContextHolder.resetRequestAttributes();
    }

    @Test
    void forwardsTheInboundAuthorizationHeader() {
        MockHttpServletRequest inbound = new MockHttpServletRequest();
        inbound.addHeader(HttpHeaders.AUTHORIZATION, "Bearer caller-token");
        RequestContextHolder.setRequestAttributes(new ServletRequestAttributes(inbound));
        server.expect(requestTo("http://account-service/api/v1/accounts/1"))
                .andExpect(header(HttpHeaders.AUTHORIZATION, "Bearer caller-token"))
                .andRespond(withSuccess());

        client.get().uri("/api/v1/accounts/1").retrieve().toBodilessEntity();
        server.verify();
    }

    @Test
    void sendsNoTokenOutsideARequest() {
        server.expect(requestTo("http://account-service/api/v1/accounts/1"))
                .andExpect(headerDoesNotExist(HttpHeaders.AUTHORIZATION))
                .andRespond(withSuccess());

        client.get().uri("/api/v1/accounts/1").retrieve().toBodilessEntity();
        server.verify();
    }
}
