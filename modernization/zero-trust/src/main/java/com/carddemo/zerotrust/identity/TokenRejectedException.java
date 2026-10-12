package com.carddemo.zerotrust.identity;

/** A presented token or client assertion failed validation; the request is rejected. */
public class TokenRejectedException extends RuntimeException {

    public TokenRejectedException(String reason) {
        super(reason);
    }
}
