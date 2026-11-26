package com.carddemo.programs.cbact02c.io;

/** Two-byte COBOL FILE STATUS values produced by the {@link CardFile} implementations. */
public final class FileStatus {

    public static final String SUCCESS = "00";
    public static final String RECORD_LENGTH_MISMATCH = "04";
    public static final String END_OF_FILE = "10";
    public static final String PERMANENT_ERROR = "30";
    public static final String FILE_NOT_FOUND = "35";
    public static final String ALREADY_OPEN = "41";
    public static final String CLOSE_NOT_OPEN = "42";
    public static final String READ_NOT_OPEN = "47";

    private FileStatus() {
    }
}
