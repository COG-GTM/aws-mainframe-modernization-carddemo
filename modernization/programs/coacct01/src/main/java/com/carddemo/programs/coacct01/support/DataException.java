package com.carddemo.programs.coacct01.support;

/** S0C7 data exception: invalid digit in a numeric DISPLAY field used in arithmetic or comparison. */
public final class DataException extends RuntimeException {

    public DataException(String message) {
        super(message);
    }
}
