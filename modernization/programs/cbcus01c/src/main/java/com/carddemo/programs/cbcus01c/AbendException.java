package com.carddemo.programs.cbcus01c;

/** Raised where the COBOL program calls CEE3ABD (Z-ABEND-PROGRAM). */
public final class AbendException extends RuntimeException {

    private final int abendCode;
    private final int timing;

    public AbendException(int abendCode, int timing) {
        super("CEE3ABD ABCODE=" + abendCode + " TIMING=" + timing);
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
