package com.carddemo.programs.cbtrn01c;

/** CALL 'CEE3ABD' USING ABCODE, TIMING: user abend; the program terminates without closing files. */
public final class AbendException extends RuntimeException {

    private final int abendCode;
    private final int timing;

    public AbendException(int abendCode, int timing) {
        super("CBTRN01C abended with code U" + abendCode);
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
