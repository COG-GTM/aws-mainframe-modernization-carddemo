package com.carddemo.programs.cocrdlic;

/** A CICS condition raised on a command without RESP(), which ends the task with the given abend code. */
public class CicsAbendException extends RuntimeException {
    private final String abendCode;

    public CicsAbendException(String abendCode, String message) {
        super(abendCode + ": " + message);
        this.abendCode = abendCode;
    }

    public String abendCode() {
        return abendCode;
    }
}
