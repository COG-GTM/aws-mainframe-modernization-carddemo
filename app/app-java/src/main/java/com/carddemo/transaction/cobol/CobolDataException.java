package com.carddemo.transaction.cobol;

/**
 * Invalid numeric data - the equivalent of an S0C7 data exception abend.
 */
public class CobolDataException extends RuntimeException {

    public CobolDataException(String message) {
        super("S0C7 DATA EXCEPTION: " + message);
    }
}
