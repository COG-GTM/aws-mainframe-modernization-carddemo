package com.carddemo.batch.io;

/**
 * The Java equivalent of {@code CALL 'CEE3ABD' USING ABCODE, TIMING}: an unrecoverable user abend.
 * CBACT01C always uses abend code 999 and timing 0 (no dump). The job step return code is the abend code.
 */
public class AbendException extends RuntimeException {
    private static final long serialVersionUID = 1L;

    private final int abendCode;
    private final int timing;

    public AbendException(int abendCode, int timing, Throwable cause) {
        super("CEE3ABD: USER ABEND U" + abendCode + " TIMING=" + timing, cause);
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
