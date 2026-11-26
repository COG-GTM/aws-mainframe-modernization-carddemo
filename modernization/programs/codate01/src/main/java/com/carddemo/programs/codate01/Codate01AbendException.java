package com.carddemo.programs.codate01;

/** The CICS task would ABEND with {@code abendCode}. */
public final class Codate01AbendException extends RuntimeException {

    private final String abendCode;
    private final String errDisplay;

    public Codate01AbendException(String abendCode, String message, String errDisplay) {
        super(abendCode + ": " + message);
        this.abendCode = abendCode;
        this.errDisplay = errDisplay;
    }

    /** MQ-ERR-DISPLAY at the time of the abend. */
    public String errDisplay() {
        return errDisplay;
    }

    public String abendCode() {
        return abendCode;
    }
}
