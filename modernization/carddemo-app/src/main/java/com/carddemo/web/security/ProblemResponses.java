package com.carddemo.web.security;

import com.carddemo.common.web.ApiErrors;
import com.carddemo.user.menu.MenuService;
import com.carddemo.user.signon.SignOnService;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.net.URI;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ProblemDetail;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.AuthenticationEntryPoint;
import org.springframework.security.web.access.AccessDeniedHandler;

/**
 * Security failures in the uniform error body (ADR-0019). No/invalid token: 401 {@code SIGNON_REQUIRED} with
 * {@code toProgram = COSGN00C} (a menu entered without a COMMAREA returns to sign-on, COMEN01C/COADM01C R-1).
 * Wrong role: 403 {@code NOTAUTH} with the COMEN01C admin-only message.
 */
public class ProblemResponses implements AuthenticationEntryPoint, AccessDeniedHandler {

    public static final String SIGNON_REQUIRED = "SIGNON_REQUIRED";
    public static final String MSG_SIGNON_REQUIRED = "Please sign on to CardDemo";
    public static final String NOTAUTH = "NOTAUTH";

    private final ObjectMapper mapper;

    public ProblemResponses(ObjectMapper mapper) {
        this.mapper = mapper;
    }

    @Override
    public void commence(HttpServletRequest request, HttpServletResponse response,
            AuthenticationException authException) throws IOException {
        ProblemDetail problem = ApiErrors.problem(HttpStatus.UNAUTHORIZED, SIGNON_REQUIRED, null,
                MSG_SIGNON_REQUIRED);
        problem.setProperty("toProgram", SignOnService.PROGRAM);
        response.setHeader(HttpHeaders.WWW_AUTHENTICATE, "Bearer");
        write(request, response, problem);
    }

    @Override
    public void handle(HttpServletRequest request, HttpServletResponse response,
            AccessDeniedException accessDeniedException) throws IOException {
        ProblemDetail problem = ApiErrors.problem(HttpStatus.FORBIDDEN, NOTAUTH, null, MenuService.MSG_ADMIN_ONLY);
        problem.setProperty("cicsResp", NOTAUTH);
        write(request, response, problem);
    }

    private void write(HttpServletRequest request, HttpServletResponse response, ProblemDetail problem)
            throws IOException {
        problem.setInstance(URI.create(request.getRequestURI()));
        response.setStatus(problem.getStatus());
        response.setContentType(MediaType.APPLICATION_PROBLEM_JSON_VALUE);
        mapper.writeValue(response.getOutputStream(), problem);
    }
}
