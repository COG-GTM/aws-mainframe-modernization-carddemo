package com.carddemo.common.error;

/** FILE STATUS 23 (INVALID KEY) where the program does not handle the missing record itself. */
public class RecordNotFoundException extends FileStatusException {

    public RecordNotFoundException(String ddName, String operation, Throwable cause) {
        super(FileStatus.RECORD_NOT_FOUND, ddName, operation, cause);
    }
}
