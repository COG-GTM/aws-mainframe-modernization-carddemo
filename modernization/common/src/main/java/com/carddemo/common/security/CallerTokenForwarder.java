package com.carddemo.common.security;

import java.io.IOException;
import java.util.concurrent.Callable;
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
 * outside a request are sent without a token (and get 401), except in a command-line batch process
 * ({@link #forBatchProcess}), which has no caller and signs as {@link #BATCH_USER_ID} with user type
 * {@code U}, the identity a scheduler-submitted job runs under. Work handed to another thread on behalf
 * of a request (an online-submitted report) runs inside {@link #onBehalfOf} with that request's header.
 */
public class CallerTokenForwarder implements ClientHttpRequestInterceptor {

    public static final String BATCH_USER_ID = "BATCH";

    private static final ThreadLocal<String> DELEGATED = new ThreadLocal<>();

    private final TokenService tokens;

    private CallerTokenForwarder(TokenService tokens) {
        this.tokens = tokens;
    }

    /** For a web service: forwards the caller's token, never invents one. */
    public static CallerTokenForwarder forCallers() {
        return new CallerTokenForwarder(null);
    }

    /** For a {@code --job=} process (no web server, so no callers): every call carries the batch identity. */
    public static CallerTokenForwarder forBatchProcess(TokenService tokens) {
        return new CallerTokenForwarder(tokens);
    }

    /**
     * Runs {@code work} on the current (non-request) thread forwarding {@code authorization}, the header
     * captured from the request that submitted it, so the services still see the original caller.
     */
    public static <T> T onBehalfOf(String authorization, Callable<T> work) throws Exception {
        String previous = DELEGATED.get();
        DELEGATED.set(authorization);
        try {
            return work.call();
        } finally {
            if (previous == null) {
                DELEGATED.remove();
            } else {
                DELEGATED.set(previous);
            }
        }
    }

    @Override
    public ClientHttpResponse intercept(HttpRequest request, byte[] body, ClientHttpRequestExecution execution)
            throws IOException {
        if (!request.getHeaders().containsKey(HttpHeaders.AUTHORIZATION)) {
            String authorization = DELEGATED.get() != null ? DELEGATED.get()
                    : RequestContextHolder.getRequestAttributes() instanceof ServletRequestAttributes attributes
                    ? attributes.getRequest().getHeader(HttpHeaders.AUTHORIZATION)
                    : tokens != null ? "Bearer " + tokens.issue(BATCH_USER_ID, CallerIdentity.USER).token() : null;
            if (authorization != null) {
                request.getHeaders().set(HttpHeaders.AUTHORIZATION, authorization);
            }
        }
        return execution.execute(request, body);
    }
}
