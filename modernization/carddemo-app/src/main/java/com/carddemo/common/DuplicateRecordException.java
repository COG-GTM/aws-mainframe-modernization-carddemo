package com.carddemo.common;

/** {@code DFHRESP(DUPREC)} / {@code DFHRESP(DUPKEY)} / file status 22: the key already exists (HTTP 409, ADR-0009). */
public class DuplicateRecordException extends RuntimeException {

    public DuplicateRecordException(String message) {
        super(message);
    }
}
