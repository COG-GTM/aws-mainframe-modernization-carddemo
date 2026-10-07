package com.carddemo.batch.io;

/**
 * A COBOL FILE STATUS other than {@code 00} raised by an OPEN / READ / WRITE / CLOSE. The two-character
 * status follows the ANSI codes the programs test for: {@code 10} end of file, {@code 22} duplicate key,
 * {@code 23} record not found, {@code 30} permanent I/O error, {@code 35} file not found on OPEN,
 * {@code 39} file attribute (record length) mismatch, {@code 42} CLOSE of a file that is not open.
 */
public class FileStatusException extends RuntimeException {
    private static final long serialVersionUID = 1L;

    public static final String END_OF_FILE = "10";
    public static final String DUPLICATE_KEY = "22";
    public static final String RECORD_NOT_FOUND = "23";
    public static final String PERMANENT_ERROR = "30";
    public static final String FILE_NOT_FOUND = "35";
    public static final String ATTRIBUTE_MISMATCH = "39";
    public static final String NOT_OPEN = "42";

    private final String ddname;
    private final String operation;
    private final String status;

    public FileStatusException(String ddname, String operation, String status) {
        this(ddname, operation, status, null);
    }

    public FileStatusException(String ddname, String operation, String status, Throwable cause) {
        super(operation + " " + ddname + " file status " + status + (cause == null ? "" : ": " + cause.getMessage()),
                cause);
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

    /** The two-character FILE STATUS value. */
    public String status() {
        return status;
    }
}
