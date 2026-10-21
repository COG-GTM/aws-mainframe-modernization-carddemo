package com.carddemo.batch.posttran.cobol;

/**
 * Equivalent of a failing file-status check followed by 9999-ABEND-PROGRAM (CEE3ABD, U999):
 * carries the DISPLAY text and the two-character file status.
 */
public class Abend extends RuntimeException {

    private final String fileStatus;

    public Abend(String message, String fileStatus) {
        this(message, fileStatus, null);
    }

    public Abend(String message, String fileStatus, Throwable cause) {
        super(message, cause);
        this.fileStatus = fileStatus;
    }

    public String fileStatus() {
        return fileStatus;
    }

    /** 9910-DISPLAY-IO-STATUS for a numeric status. */
    public String ioStatusLine() {
        return "FILE STATUS IS: NNNN00" + fileStatus;
    }
}
