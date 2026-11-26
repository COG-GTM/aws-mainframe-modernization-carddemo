package com.carddemo.programs.cbimport;

/** WS-FILE-STATUS-AREA: two-character COBOL file status values and their 88-levels. */
public final class FileStatus {

    /** 88 WS-xxx-OK VALUE '00'. */
    public static final String OK = "00";
    /** 88 WS-EXPORT-EOF VALUE '10'. */
    public static final String EOF = "10";
    /** Wrong-length record. */
    public static final String RECORD_LENGTH_CONFLICT = "04";
    /** Permanent I/O error. */
    public static final String PERMANENT_ERROR = "30";
    /** OPEN on a file whose DD statement / dataset is not present. */
    public static final String FILE_NOT_FOUND = "35";
    /** WRITE with a record length outside the file's fixed length. */
    public static final String BOUNDARY_VIOLATION = "44";

    private FileStatus() {
    }

    public static boolean isOk(String status) {
        return OK.equals(status);
    }

    public static boolean isEof(String status) {
        return EOF.equals(status);
    }
}
