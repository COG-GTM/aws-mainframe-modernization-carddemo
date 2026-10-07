package com.carddemo.common;

/**
 * {@code DFHRESP(INVREQ)} or a failed screen edit: the request itself is invalid (HTTP 400, ADR-0009).
 * {@link #field()} names the request field the edit failed on (the BMS field the COBOL put the cursor on), or
 * {@code null}.
 */
public class InvalidRequestException extends RuntimeException {

    private final String field;

    public InvalidRequestException(String message) {
        this(null, message);
    }

    public InvalidRequestException(String field, String message) {
        super(message);
        this.field = field;
    }

    public String field() {
        return field;
    }
}
