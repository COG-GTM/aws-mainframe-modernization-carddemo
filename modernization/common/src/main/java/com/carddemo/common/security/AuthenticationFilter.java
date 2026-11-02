package com.carddemo.common.security;

import com.carddemo.common.api.ApiError;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.List;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.util.AntPathMatcher;
import org.springframework.web.filter.OncePerRequestFilter;

/**
 * Requires a valid bearer token on every {@code /api/**} request except the configured public paths
 * (sign-on). Actuator, OpenAPI and Swagger UI are outside {@code /api/} and stay open. A missing or
 * invalid token gets 401 as {@link ApiError}; a valid one is exposed as {@link CallerIdentity}.
 */
public class AuthenticationFilter extends OncePerRequestFilter {

    static final String MISSING = "Authentication required. Sign on first ...";
    static final String INVALID = "Session token is invalid or expired. Sign on again ...";

    private static final String BEARER = "Bearer ";
    private final AntPathMatcher matcher = new AntPathMatcher();
    private final TokenService tokens;
    private final List<String> publicPaths;
    private final ObjectMapper objectMapper;

    public AuthenticationFilter(TokenService tokens, List<String> publicPaths, ObjectMapper objectMapper) {
        this.tokens = tokens;
        this.publicPaths = List.copyOf(publicPaths);
        this.objectMapper = objectMapper;
    }

    @Override
    protected boolean shouldNotFilter(HttpServletRequest request) {
        String path = request.getRequestURI().substring(request.getContextPath().length());
        if (!path.startsWith("/api/")) {
            return true;
        }
        return publicPaths.stream().anyMatch(pattern -> matcher.match(pattern, path));
    }

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain chain)
            throws ServletException, IOException {
        String header = request.getHeader(HttpHeaders.AUTHORIZATION);
        if (header == null || !header.regionMatches(true, 0, BEARER, 0, BEARER.length())) {
            reject(response, MISSING);
            return;
        }
        CallerIdentity caller;
        try {
            caller = tokens.verify(header.substring(BEARER.length()).trim());
        } catch (InvalidTokenException e) {
            reject(response, INVALID);
            return;
        }
        request.setAttribute(CallerIdentity.REQUEST_ATTRIBUTE, caller);
        chain.doFilter(request, response);
    }

    private void reject(HttpServletResponse response, String message) throws IOException {
        response.setStatus(HttpStatus.UNAUTHORIZED.value());
        response.setHeader(HttpHeaders.WWW_AUTHENTICATE, "Bearer");
        response.setContentType(MediaType.APPLICATION_JSON_VALUE);
        objectMapper.writeValue(response.getOutputStream(), ApiError.of(401, "Unauthorized", message));
    }
}
