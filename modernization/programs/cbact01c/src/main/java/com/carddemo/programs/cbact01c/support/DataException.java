package com.carddemo.programs.cbact01c.support;

/** Invalid numeric data in a DISPLAY or COMP-3 field (the mainframe would raise S0C7). */
public class DataException extends RuntimeException {
    public DataException(String message) {
        super(message);
    }
}
