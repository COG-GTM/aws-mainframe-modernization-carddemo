package com.carddemo.common.security;

/** The bearer token is malformed, not signed with the shared key, expired or lacks the claims. */
public class InvalidTokenException extends RuntimeException {

    public InvalidTokenException(String message, Throwable cause) {
        super(message, cause);
    }
}
