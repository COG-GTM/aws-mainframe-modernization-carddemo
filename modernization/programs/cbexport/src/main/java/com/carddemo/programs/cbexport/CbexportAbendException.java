package com.carddemo.programs.cbexport;

/**
 * Raised by 9999-ABEND-PROGRAM, the Java stand-in for {@code CALL 'CEE3ABD'}. The message is the
 * last ERROR line DISPLAYed before the abend.
 */
public final class CbexportAbendException extends RuntimeException {

    private final String fileStatus;

    public CbexportAbendException(String message, String fileStatus) {
        super(message);
        this.fileStatus = fileStatus;
    }

    public String fileStatus() {
        return fileStatus;
    }
}
