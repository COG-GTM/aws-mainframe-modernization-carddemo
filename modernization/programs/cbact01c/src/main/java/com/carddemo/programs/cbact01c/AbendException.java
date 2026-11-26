package com.carddemo.programs.cbact01c;

/** {@code CALL 'CEE3ABD' USING ABCODE, TIMING} from 9999-ABEND-PROGRAM: the run ends with user abend U0999. */
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
