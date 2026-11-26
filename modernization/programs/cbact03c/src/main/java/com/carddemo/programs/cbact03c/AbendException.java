package com.carddemo.programs.cbact03c;

import java.util.Locale;

/** CALL 'CEE3ABD' USING ABCODE, TIMING: unrecoverable termination with a user abend code. */
public class AbendException extends RuntimeException {

    private final int abendCode;
    private final int timing;

    public AbendException(int abendCode, int timing) {
        super("ABEND U" + String.format(Locale.ROOT, "%04d", abendCode));
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
