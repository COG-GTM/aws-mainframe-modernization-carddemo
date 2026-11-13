package com.carddemo.common.cobol;

/** Raised when bytes do not hold a valid value for the COBOL picture/usage they are decoded with. */
public class CobolDataException extends RuntimeException {

    public CobolDataException(String message) {
        super(message);
    }

    public CobolDataException(String message, Throwable cause) {
        super(message, cause);
    }
}
