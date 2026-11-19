package com.carddemo.common.error;

/** FILE STATUS 22: a WRITE with a key that already exists (e.g. CBTRN02C {@code 2900-WRITE-TRANSACTION-FILE}). */
public class DuplicateKeyException extends FileStatusException {

    public DuplicateKeyException(String ddName, String operation, Throwable cause) {
        super(FileStatus.DUPLICATE_KEY, ddName, operation, cause);
    }
}
