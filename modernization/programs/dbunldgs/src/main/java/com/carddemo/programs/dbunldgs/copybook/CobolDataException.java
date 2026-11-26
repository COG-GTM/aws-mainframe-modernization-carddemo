package com.carddemo.programs.dbunldgs.copybook;

/** Invalid numeric data in a COBOL field (the condition that raises S0C7 on the mainframe). */
public class CobolDataException extends RuntimeException {
    public CobolDataException(String message) {
        super(message);
    }
}
