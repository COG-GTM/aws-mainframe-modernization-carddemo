package com.carddemo.poc.batch;

/**
 * Java equivalent of paragraph {@code 9999-ABEND-PROGRAM}
 * ({@code CALL 'CEE3ABD' USING ABCODE, TIMING} with ABCODE 999).
 */
public final class AbendException extends RuntimeException {

    public static final int ABEND_CODE = 999;

    public AbendException(String message) {
        super(message);
    }

    public int abendCode() {
        return ABEND_CODE;
    }
}
