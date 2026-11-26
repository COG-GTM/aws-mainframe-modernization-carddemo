package com.carddemo.programs.cbact03c;

/** Two-byte COBOL FILE STATUS values used by the XREFFILE gateway. */
public final class FileStatus {

    public static final String OK = "00";
    public static final String RECORD_LENGTH_MISMATCH = "04";
    public static final String END_OF_FILE = "10";
    public static final String DUPLICATE_KEY = "22";
    public static final String PERMANENT_IO_ERROR = "30";
    public static final String FILE_NOT_FOUND = "35";
    public static final String NOT_OPEN = "42";
    public static final String READ_NOT_OPEN = "47";

    private FileStatus() {
    }
}
