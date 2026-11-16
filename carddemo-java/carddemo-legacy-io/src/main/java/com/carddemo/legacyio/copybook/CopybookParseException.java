package com.carddemo.legacyio.copybook;

/** A copybook uses syntax the parser does not understand. */
public class CopybookParseException extends RuntimeException {

    public CopybookParseException(String message) {
        super(message);
    }
}
