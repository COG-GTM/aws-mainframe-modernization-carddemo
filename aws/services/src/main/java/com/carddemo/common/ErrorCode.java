package com.carddemo.common;

import org.springframework.http.HttpStatus;

public enum ErrorCode {
    VALIDATION_ERROR(HttpStatus.BAD_REQUEST),
    INVALID_REQUEST(HttpStatus.BAD_REQUEST),
    INVALID_CREDENTIALS(HttpStatus.UNAUTHORIZED),
    UNAUTHENTICATED(HttpStatus.UNAUTHORIZED),
    FORBIDDEN(HttpStatus.FORBIDDEN),
    NOT_FOUND(HttpStatus.NOT_FOUND),
    DUPLICATE(HttpStatus.CONFLICT),
    CONCURRENT_UPDATE(HttpStatus.CONFLICT),
    LOCKED(HttpStatus.CONFLICT),
    INTEGRITY_VIOLATION(HttpStatus.CONFLICT),
    BUSINESS_RULE(HttpStatus.UNPROCESSABLE_ENTITY),
    INTERNAL_ERROR(HttpStatus.INTERNAL_SERVER_ERROR);

    private final HttpStatus status;

    ErrorCode(HttpStatus status) {
        this.status = status;
    }

    public HttpStatus status() {
        return status;
    }
}
