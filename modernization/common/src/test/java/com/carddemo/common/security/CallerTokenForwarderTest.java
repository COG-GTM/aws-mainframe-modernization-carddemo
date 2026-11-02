package com.carddemo.common.security;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.client.match.MockRestRequestMatchers.header;
import static org.springframework.test.web.client.match.MockRestRequestMatchers.headerDoesNotExist;
import static org.springframework.test.web.client.match.MockRestRequestMatchers.requestTo;
import static org.springframework.test.web.client.response.MockRestResponseCreators.withSuccess;

import java.time.Clock;
import java.time.Duration;
import java.util.List;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpHeaders;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.test.web.client.MockRestServiceServer;
import org.springframework.web.client.RestClient;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

class CallerTokenForwarderTest {

    private final TokenService tokens = new TokenService(
            new SecurityProperties("test-only-token-secret-0123456789abcdef", Duration.ofHours(8), "carddemo-auth-service", List.of()),
            Clock.systemUTC());
    private final RestClient.Builder builder = RestClient.builder().baseUrl("http://account-service")
            .requestInterceptor(CallerTokenForwarder.forCallers());
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
    void sendsNoTokenOutsideARequestInAWebService() {
        server.expect(requestTo("http://account-service/api/v1/accounts/1"))
                .andExpect(headerDoesNotExist(HttpHeaders.AUTHORIZATION))
                .andRespond(withSuccess());

        client.get().uri("/api/v1/accounts/1").retrieve().toBodilessEntity();
        server.verify();
    }

    @Test
    void signsAsTheBatchUserInABatchProcess() {
        RestClient.Builder batchBuilder = RestClient.builder().baseUrl("http://account-service")
                .requestInterceptor(CallerTokenForwarder.forBatchProcess(tokens));
        MockRestServiceServer server = MockRestServiceServer.bindTo(batchBuilder).build();
        RestClient client = batchBuilder.build();
        server.expect(requestTo("http://account-service/api/v1/accounts/1"))
                .andExpect(request -> {
                    String authorization = request.getHeaders().getFirst(HttpHeaders.AUTHORIZATION);
                    assertThat(authorization).startsWith("Bearer ");
                    CallerIdentity caller = tokens.verify(authorization.substring("Bearer ".length()));
                    assertThat(caller).isEqualTo(new CallerIdentity("BATCH", "U"));
                })
                .andRespond(withSuccess());

        client.get().uri("/api/v1/accounts/1").retrieve().toBodilessEntity();
        server.verify();
    }
}
