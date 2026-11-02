package com.carddemo.common.error;

/** The caller is signed on but its user type may not use the endpoint (COMEN01C "Admin Only"). */
public class ForbiddenException extends RuntimeException {

    public ForbiddenException(String message) {
        super(message);
    }
}
