package com.carddemo.programs.cobil00c.gateway;

/** A CICS command failed with no RESP option (unhandled condition): the task abends. */
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
