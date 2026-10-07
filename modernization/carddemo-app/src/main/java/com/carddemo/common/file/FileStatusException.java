package com.carddemo.common.file;

import com.carddemo.common.AbendException;

/**
 * A file operation that ended with an unexpected status. The CardDemo batch programs display the status and
 * {@code CALL 'CEE3ABD'} with code 999, so this is an {@link AbendException} with the CardDemo abend code.
 */
public class FileStatusException extends AbendException {

    private final String ddname;
    private final String operation;
    private final FileStatus status;

    public FileStatusException(String ddname, String operation, FileStatus status) {
        this(ddname, operation, status, null);
    }

    public FileStatusException(String ddname, String operation, FileStatus status, Throwable cause) {
        super(CARDDEMO_ABEND_CODE, "ERROR " + operation + " " + ddname + " FILE STATUS " + status.code(), cause);
        this.ddname = ddname;
        this.operation = operation;
        this.status = status;
    }

    public String ddname() {
        return ddname;
    }

    public String operation() {
        return operation;
    }

    public FileStatus status() {
        return status;
    }
}
