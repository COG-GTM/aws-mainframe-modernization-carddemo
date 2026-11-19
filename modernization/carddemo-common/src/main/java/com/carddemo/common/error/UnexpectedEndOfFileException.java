package com.carddemo.common.error;

/** FILE STATUS 10 where end of file is not an expected outcome of the operation. */
public class UnexpectedEndOfFileException extends FileStatusException {

    public UnexpectedEndOfFileException(String ddName, String operation, Throwable cause) {
        super(FileStatus.END_OF_FILE, ddName, operation, cause);
    }
}
