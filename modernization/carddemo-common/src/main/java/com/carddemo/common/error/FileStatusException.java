package com.carddemo.common.error;

import java.util.Objects;

/**
 * An I/O outcome the program did not expect (CORE error handling, REF conventions §8). It is an
 * {@link AbendException}: the step fails and the launch exits 16; the record is never skipped.
 */
public class FileStatusException extends AbendException {

    private final FileStatus status;
    private final String ddName;
    private final String operation;

    public FileStatusException(FileStatus status, String ddName, String operation) {
        this(status, ddName, operation, null);
    }

    public FileStatusException(FileStatus status, String ddName, String operation, Throwable cause) {
        super(operation + " " + ddName + " failed: FILE STATUS " + Objects.requireNonNull(status, "status"), cause);
        this.status = status;
        this.ddName = ddName;
        this.operation = operation;
    }

    /** The most specific exception type for {@code status}. */
    public static FileStatusException of(FileStatus status, String ddName, String operation) {
        return of(status, ddName, operation, null);
    }

    public static FileStatusException of(FileStatus status, String ddName, String operation, Throwable cause) {
        if (FileStatus.DUPLICATE_KEY.equals(status)) {
            return new DuplicateKeyException(ddName, operation, cause);
        }
        if (FileStatus.RECORD_NOT_FOUND.equals(status)) {
            return new RecordNotFoundException(ddName, operation, cause);
        }
        if (FileStatus.END_OF_FILE.equals(status)) {
            return new UnexpectedEndOfFileException(ddName, operation, cause);
        }
        return new FileStatusException(status, ddName, operation, cause);
    }

    public FileStatus status() {
        return status;
    }

    public String ddName() {
        return ddName;
    }

    public String operation() {
        return operation;
    }
}
