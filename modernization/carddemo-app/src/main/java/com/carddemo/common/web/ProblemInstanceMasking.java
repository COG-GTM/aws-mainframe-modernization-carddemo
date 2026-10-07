package com.carddemo.common.web;

import com.carddemo.common.PanMask;
import java.net.URI;
import org.springframework.core.MethodParameter;
import org.springframework.http.MediaType;
import org.springframework.http.ProblemDetail;
import org.springframework.http.converter.HttpMessageConverter;
import org.springframework.http.server.ServerHttpRequest;
import org.springframework.http.server.ServerHttpResponse;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.servlet.mvc.method.annotation.ResponseBodyAdvice;

/**
 * Error bodies echo the request path as RFC 7807 {@code instance}; a card number typed into
 * {@code /api/v1/cards/<pan>} is masked there (ADR-0020, s6.4), like in list responses and log lines.
 */
@RestControllerAdvice
public class ProblemInstanceMasking implements ResponseBodyAdvice<Object> {

    @Override
    public boolean supports(MethodParameter returnType, Class<? extends HttpMessageConverter<?>> converterType) {
        return true;
    }

    @Override
    public Object beforeBodyWrite(Object body, MethodParameter returnType, MediaType contentType,
            Class<? extends HttpMessageConverter<?>> converterType, ServerHttpRequest request,
            ServerHttpResponse response) {
        if (body instanceof ProblemDetail problem) {
            String path = problem.getInstance() == null ? request.getURI().getRawPath()
                    : problem.getInstance().toString();
            problem.setInstance(URI.create(PanMask.maskCardPath(path)));
        }
        return body;
    }
}
