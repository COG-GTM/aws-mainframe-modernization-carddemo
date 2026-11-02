package com.carddemo.common.security;

import java.io.IOException;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpRequest;
import org.springframework.http.client.ClientHttpRequestExecution;
import org.springframework.http.client.ClientHttpRequestInterceptor;
import org.springframework.http.client.ClientHttpResponse;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

/**
 * Copies the {@code Authorization} header of the inbound request bound to the current thread onto an
 * outbound service-to-service call, so account/card services see the original caller. Calls made
 * outside a request (a batch job started from the command line) carry a token minted for
 * {@link #BATCH_USER_ID} with user type {@code U}, the identity a scheduler-submitted job runs under.
 */
public class CallerTokenForwarder implements ClientHttpRequestInterceptor {

    public static final String BATCH_USER_ID = "BATCH";

    private final TokenService tokens;

    public CallerTokenForwarder(TokenService tokens) {
        this.tokens = tokens;
    }

    @Override
    public ClientHttpResponse intercept(HttpRequest request, byte[] body, ClientHttpRequestExecution execution)
            throws IOException {
        if (!request.getHeaders().containsKey(HttpHeaders.AUTHORIZATION)) {
            String authorization = RequestContextHolder.getRequestAttributes() instanceof ServletRequestAttributes attributes
                    ? attributes.getRequest().getHeader(HttpHeaders.AUTHORIZATION)
                    : "Bearer " + tokens.issue(BATCH_USER_ID, CallerIdentity.USER).token();
            if (authorization != null) {
                request.getHeaders().set(HttpHeaders.AUTHORIZATION, authorization);
            }
        }
        return execution.execute(request, body);
    }
}
