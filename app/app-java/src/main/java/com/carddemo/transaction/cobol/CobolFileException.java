package com.carddemo.transaction.cobol;

/**
 * Replaces {@code Z-DISPLAY-IO-STATUS} + {@code Z-ABEND-PROGRAM}: an unrecoverable I/O condition on a
 * file (identified by its DD name) carrying the COBOL file status. Throwing it fails the Spring Batch
 * step and job, the equivalent of the {@code CEE3ABD} abend (ABCODE 999).
 */
public class CobolFileException extends RuntimeException {

    public static final int ABEND_CODE = 999;

    private final String ddName;
    private final String fileStatus;

    public CobolFileException(String ddName, FileStatus status, String message, Throwable cause) {
        this(ddName, status.code(), message, cause);
    }

    public CobolFileException(String ddName, String fileStatus, String message, Throwable cause) {
        super(message + " [DD " + ddName + "] " + FileStatus.displayIoStatus(fileStatus)
                + " - ABENDING PROGRAM (ABCODE " + ABEND_CODE + ")", cause);
        this.ddName = ddName;
        this.fileStatus = fileStatus;
    }

    public String getDdName() {
        return ddName;
    }

    public String getFileStatus() {
        return fileStatus;
    }
}
