package com.carddemo.common.error;

/** Equivalent of {@code DFHRESP(NOTFND)} / VSAM file status 23. */
public class NotFoundException extends RuntimeException {

    public NotFoundException(String message) {
        super(message);
    }
}
