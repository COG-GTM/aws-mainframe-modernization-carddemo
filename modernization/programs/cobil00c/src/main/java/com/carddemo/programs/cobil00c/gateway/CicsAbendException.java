package com.carddemo.programs.cobil00c.gateway;

import java.util.List;

/** A CICS command failed with no RESP option (unhandled condition): the task abends. */
public class CicsAbendException extends RuntimeException {
    private final String abendCode;
    private final String detail;
    private final List<String> displayLines;

    public CicsAbendException(String abendCode, String message) {
        this(abendCode, message, List.of(), null);
    }

    private CicsAbendException(String abendCode, String detail, List<String> displayLines, Throwable cause) {
        super(abendCode + ": " + detail, cause);
        this.abendCode = abendCode;
        this.detail = detail;
        this.displayLines = List.copyOf(displayLines);
    }

    /** Same abend, carrying the DISPLAY (SYSOUT) lines the task wrote before abending. */
    public CicsAbendException withDisplayLines(List<String> lines) {
        return new CicsAbendException(abendCode, detail, lines, this);
    }

    public String abendCode() {
        return abendCode;
    }

    public List<String> displayLines() {
        return displayLines;
    }
}
