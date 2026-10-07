package com.carddemo.common;

/** {@code DFHRESP(INVREQ)} or a failed screen edit: the request itself is invalid (HTTP 400, ADR-0009). */
public class InvalidRequestException extends RuntimeException {

    public InvalidRequestException(String message) {
        super(message);
    }
}
