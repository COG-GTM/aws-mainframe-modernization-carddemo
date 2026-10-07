package com.carddemo.common.web;

import org.springframework.http.HttpStatusCode;
import org.springframework.http.ProblemDetail;

/** Builds the uniform {@link ApiError} body (ADR-0019) on top of Spring's {@link ProblemDetail}. */
public final class ApiErrors {

    public static final String CODE = "code";
    public static final String FIELD = "field";
    public static final String MESSAGE = "message";

    private ApiErrors() {
    }

    public static ProblemDetail problem(HttpStatusCode status, String code, String field, String message) {
        return decorate(ProblemDetail.forStatus(status), code, field, message);
    }

    /** Sets {@code code}, {@code field} (also when null, so the member is always present) and {@code message}. */
    public static ProblemDetail decorate(ProblemDetail problem, String code, String field, String message) {
        problem.setDetail(message);
        problem.setProperty(CODE, code);
        problem.setProperty(FIELD, field);
        problem.setProperty(MESSAGE, message);
        return problem;
    }

    public static boolean isDecorated(ProblemDetail problem) {
        return problem.getProperties() != null && problem.getProperties().containsKey(CODE);
    }
}
