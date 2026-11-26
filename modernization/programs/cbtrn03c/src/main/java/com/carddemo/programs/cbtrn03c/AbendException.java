package com.carddemo.programs.cbtrn03c;

/** 9999-ABEND-PROGRAM: CALL 'CEE3ABD' USING ABCODE (999), TIMING (0). */
public final class AbendException extends RuntimeException {
    private final int abendCode;

    public AbendException(int abendCode, String reason) {
        super("CBTRN03C abended with code " + abendCode + ": " + reason);
        this.abendCode = abendCode;
    }

    public int abendCode() {
        return abendCode;
    }
}
