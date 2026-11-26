package com.carddemo.programs.cbexport.io;

/** COBOL FILE STATUS codes used by CBEXPORT and its readers/writers. */
public final class FileStatus {

    /** 88 WS-xxx-OK VALUE '00'. */
    public static final String OK = "00";
    /** Record length does not match the fixed-length file definition. */
    public static final String LENGTH_MISMATCH = "04";
    /** 88 WS-xxx-EOF VALUE '10'. */
    public static final String EOF = "10";
    /** Sequence error: KSDS key not ascending on sequential WRITE. */
    public static final String SEQUENCE_ERROR = "21";
    /** Permanent I/O error. */
    public static final String PERMANENT_ERROR = "30";
    /** OPEN on a file that does not exist. */
    public static final String FILE_NOT_FOUND = "35";
    /** WRITE record length inconsistent with the file. */
    public static final String RECORD_LENGTH_ERROR = "44";

    private FileStatus() {
    }

    public static boolean isOk(String status) {
        return OK.equals(status);
    }

    public static boolean isEof(String status) {
        return EOF.equals(status);
    }
}
