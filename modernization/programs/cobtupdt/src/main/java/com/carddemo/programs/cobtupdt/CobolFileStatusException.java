package com.carddemo.programs.cobtupdt;

/** An I/O operation that ended with a non-'00' COBOL FILE STATUS the program cannot continue from. */
public class CobolFileStatusException extends RuntimeException {

    private final String fileStatus;

    public CobolFileStatusException(String fileStatus, String message) {
        super(message + " (FILE STATUS " + fileStatus + ")");
        this.fileStatus = fileStatus;
    }

    public CobolFileStatusException(String fileStatus, String message, Throwable cause) {
        super(message + " (FILE STATUS " + fileStatus + ")", cause);
        this.fileStatus = fileStatus;
    }

    public String fileStatus() {
        return fileStatus;
    }
}
