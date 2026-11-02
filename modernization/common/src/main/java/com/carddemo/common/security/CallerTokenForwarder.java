package com.carddemo.common.security;

import jakarta.servlet.http.HttpServletRequest;
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
 * outside a request (no bound request) are sent without a token and will get 401.
 */
public class CallerTokenForwarder implements ClientHttpRequestInterceptor {

    @Override
    public ClientHttpResponse intercept(HttpRequest request, byte[] body, ClientHttpRequestExecution execution)
            throws IOException {
        if (!request.getHeaders().containsKey(HttpHeaders.AUTHORIZATION)
                && RequestContextHolder.getRequestAttributes() instanceof ServletRequestAttributes attributes) {
            HttpServletRequest inbound = attributes.getRequest();
            String authorization = inbound.getHeader(HttpHeaders.AUTHORIZATION);
            if (authorization != null) {
                request.getHeaders().set(HttpHeaders.AUTHORIZATION, authorization);
            }
        }
        return execution.execute(request, body);
    }
}
