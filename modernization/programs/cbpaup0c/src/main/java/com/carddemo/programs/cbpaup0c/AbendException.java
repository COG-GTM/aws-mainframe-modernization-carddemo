package com.carddemo.programs.cbpaup0c;

/** 9999-ABEND: the COBOL program sets RETURN-CODE 16 and GOBACKs immediately. */
public final class AbendException extends RuntimeException {

    public static final int ABEND_RETURN_CODE = 16;

    public AbendException(String reason) {
        super(reason);
    }

    public int returnCode() {
        return ABEND_RETURN_CODE;
    }
}
