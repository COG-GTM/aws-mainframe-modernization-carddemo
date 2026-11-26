package com.carddemo.programs.cbact01c.io;

/** COBOL two-character FILE STATUS values used by the readers/writers. */
public final class FileStatus {
    public static final String OK = "00";
    public static final String END_OF_FILE = "10";
    public static final String RECORD_LENGTH_MISMATCH = "04";
    public static final String PERMANENT_ERROR = "30";
    public static final String BOUNDARY_VIOLATION = "34";
    public static final String FILE_NOT_FOUND = "35";
    public static final String NOT_OPEN_FOR_CLOSE = "42";
    public static final String RECORD_SIZE_INVALID = "44";
    public static final String READ_AFTER_EOF = "46";
    public static final String NOT_OPEN_FOR_IO = "47";
    public static final String NOT_OPEN_FOR_WRITE = "48";

    private FileStatus() {
    }
}
