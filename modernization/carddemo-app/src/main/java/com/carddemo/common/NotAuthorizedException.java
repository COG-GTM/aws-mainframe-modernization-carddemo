package com.carddemo.common;

/** The signed-on user may not see the requested data: CICS {@code NOTAUTH}, HTTP 403 (ADR-0019). */
public class NotAuthorizedException extends RuntimeException {

    private final String field;

    public NotAuthorizedException(String field, String message) {
        super(message);
        this.field = field;
    }

    public String field() {
        return field;
    }
}
