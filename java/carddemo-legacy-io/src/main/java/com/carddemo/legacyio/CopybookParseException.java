package com.carddemo.legacyio;

/** A copybook entry the parser cannot lay out. */
public class CopybookParseException extends RuntimeException {

    public CopybookParseException(String copybook, String statement, String problem) {
        super("%s: %s in \"%s\"".formatted(copybook, problem, statement));
    }
}
