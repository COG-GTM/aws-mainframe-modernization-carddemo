package com.carddemo.common.error;

/** Equivalent of {@code DFHRESP(DUPREC)} / VSAM file status 22. */
public class DuplicateKeyException extends RuntimeException {

    public DuplicateKeyException(String message) {
        super(message);
    }
}
