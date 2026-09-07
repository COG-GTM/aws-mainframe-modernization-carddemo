package com.carddemo.poc.io;

/**
 * Subset of COBOL two-character FILE STATUS codes used by the CardDemo batch programs.
 */
public enum FileStatus {
    /** Successful completion. */
    SUCCESS("00"),
    /** End of file reached on sequential READ. */
    END_OF_FILE("10"),
    /** OPEN failed: file not found. */
    FILE_NOT_FOUND("35"),
    /** Permanent I/O error. */
    IO_ERROR("30"),
    /** OPEN attempted on a file already open. */
    ALREADY_OPEN("41"),
    /** CLOSE attempted on a file that is not open. */
    NOT_OPEN("42"),
    /** READ attempted on a file that is not open for input. */
    READ_NOT_OPEN("47");

    private final String code;

    FileStatus(String code) {
        this.code = code;
    }

    /** The two-character code as it appears in {@code XREFFILE-STATUS} etc. */
    public String code() {
        return code;
    }
}
