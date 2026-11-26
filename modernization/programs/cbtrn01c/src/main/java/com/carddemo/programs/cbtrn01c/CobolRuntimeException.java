package com.carddemo.programs.cbtrn01c;

/**
 * An I/O error on a keyed READ that is neither successful nor INVALID KEY. The COBOL program has
 * no USE declarative for these, so the runtime terminates the run unit (not via Z-ABEND-PROGRAM).
 */
public final class CobolRuntimeException extends RuntimeException {

    private final String fileName;
    private final String fileStatus;

    public CobolRuntimeException(String fileName, String fileStatus) {
        super("Unhandled I/O error on " + fileName + ", FILE STATUS " + fileStatus);
        this.fileName = fileName;
        this.fileStatus = fileStatus;
    }

    public String fileName() {
        return fileName;
    }

    public String fileStatus() {
        return fileStatus;
    }
}
