package com.carddemo.programs.cbact04c;

/** 9999-ABEND-PROGRAM: CALL 'CEE3ABD' USING ABCODE (999), TIMING (0). */
public class AbendException extends RuntimeException {

    private final int abendCode;
    private final int timing;

    public AbendException(int abendCode, int timing, String reason) {
        super("ABEND U" + String.format("%04d", abendCode) + ": " + reason);
        this.abendCode = abendCode;
        this.timing = timing;
    }

    public int abendCode() {
        return abendCode;
    }

    public int timing() {
        return timing;
    }
}
