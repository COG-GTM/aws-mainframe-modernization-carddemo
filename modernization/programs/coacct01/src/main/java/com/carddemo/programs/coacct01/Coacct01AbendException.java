package com.carddemo.programs.coacct01;

/** The CICS task abended (e.g. ASRA after a S0C7); the unit of work has been backed out. */
public final class Coacct01AbendException extends RuntimeException {

    private final String abendCode;

    public Coacct01AbendException(String abendCode, Throwable cause) {
        super("COACCT01 abend " + abendCode + ": " + cause.getMessage(), cause);
        this.abendCode = abendCode;
    }

    public String abendCode() {
        return abendCode;
    }
}
