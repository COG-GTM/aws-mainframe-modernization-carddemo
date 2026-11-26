package com.carddemo.programs.cbact02c;

/** CALL 'CEE3ABD' USING ABCODE, TIMING - unrecoverable termination of the batch step. */
public class AbendException extends RuntimeException {

    private final int abendCode;
    private final int timing;

    public AbendException(int abendCode, int timing) {
        super("CEE3ABD abend code " + abendCode + " timing " + timing);
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
