package com.carddemo.programs.cbtrn02c.io;

/** Two-byte COBOL FILE STATUS values used by the record readers/writers. */
public final class FileStatus {
    public static final String OK = "00";
    public static final String END_OF_FILE = "10";
    public static final String DUPLICATE_KEY = "22";
    public static final String RECORD_NOT_FOUND = "23";
    public static final String PERMANENT_ERROR = "30";
    public static final String FILE_NOT_FOUND = "35";
    public static final String OPEN_ALREADY = "41";
    public static final String CLOSE_NOT_OPEN = "42";
    public static final String READ_NOT_OPEN = "47";
    public static final String WRITE_NOT_OPEN = "48";
    public static final String REWRITE_NOT_OPEN = "49";

    private FileStatus() {
    }

    /** INVALID KEY condition: status key 1 = '2'. */
    public static boolean isInvalidKey(String status) {
        return status != null && status.length() == 2 && status.charAt(0) == '2';
    }
}
