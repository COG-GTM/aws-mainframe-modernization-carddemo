package com.carddemo.programs.cbcus01c;

/** Two-byte COBOL FILE STATUS values used by CBCUS01C (CUSTFILE-STATUS). */
public final class FileStatus {

    public static final String SUCCESS = "00";
    public static final String END_OF_FILE = "10";
    public static final String FILE_NOT_FOUND = "35";
    public static final String NOT_OPEN = "42";
    public static final String LOGIC_ERROR_READ_NOT_OPEN = "47";

    private FileStatus() {
    }
}
