package com.carddemo.programs.cobswait;

/** System ABEND S0C7 (data exception): invalid packed-decimal digit during MOVE/CVB. */
public final class DataExceptionAbend extends RuntimeException {

    public static final String ABEND_CODE = "S0C7";

    public DataExceptionAbend(String message) {
        super(ABEND_CODE + " " + message);
    }
}
