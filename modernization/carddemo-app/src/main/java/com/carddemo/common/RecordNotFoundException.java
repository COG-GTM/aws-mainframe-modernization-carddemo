package com.carddemo.common;

/** {@code DFHRESP(NOTFND)} / file status 23: the keyed record does not exist (HTTP 404, ADR-0009). */
public class RecordNotFoundException extends RuntimeException {

    public RecordNotFoundException(String message) {
        super(message);
    }
}
