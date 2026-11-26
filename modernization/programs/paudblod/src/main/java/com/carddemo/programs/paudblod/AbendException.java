package com.carddemo.programs.paudblod;

/** Raised by 9999-ABEND: {@code MOVE 16 TO RETURN-CODE, GOBACK}. */
public final class AbendException extends RuntimeException {

    public static final int RETURN_CODE = 16;

    public AbendException() {
        super("IMS LOAD ABENDING ...");
    }

    public int returnCode() {
        return RETURN_CODE;
    }
}
