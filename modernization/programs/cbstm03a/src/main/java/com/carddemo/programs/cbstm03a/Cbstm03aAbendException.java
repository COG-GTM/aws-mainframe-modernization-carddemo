package com.carddemo.programs.cbstm03a;

/** 9999-ABEND-PROGRAM: {@code CALL 'CEE3ABD'} terminates the step abnormally. */
public final class Cbstm03aAbendException extends RuntimeException {

    private final String reason;
    private final String returnCode;

    public Cbstm03aAbendException(String reason, String returnCode) {
        super(reason + (returnCode == null ? "" : " (RETURN CODE: " + returnCode + ")"));
        this.reason = reason;
        this.returnCode = returnCode;
    }

    /** The DISPLAYed error line preceding the abend, e.g. {@code ERROR READING CUSTFILE}. */
    public String reason() {
        return reason;
    }

    /** WS-M03B-RC at the time of the abend, or null when not file-related. */
    public String returnCode() {
        return returnCode;
    }
}
