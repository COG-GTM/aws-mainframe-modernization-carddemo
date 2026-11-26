package com.carddemo.programs.cbtrn03c.io;

/** Two-byte COBOL FILE STATUS values used by the I/O adapters. */
public final class FileStatus {
    public static final String OK = "00";
    public static final String END_OF_FILE = "10";
    public static final String RECORD_NOT_FOUND = "23";
    public static final String PERMANENT_ERROR = "30";
    public static final String FILE_NOT_FOUND = "35";
    public static final String NOT_OPEN_FOR_CLOSE = "42";
    public static final String NOT_OPEN_FOR_READ = "47";
    public static final String NOT_OPEN_FOR_WRITE = "48";

    private FileStatus() {
    }

    /** Status key 1 = '0': successful completion. */
    public static boolean isSuccessful(String status) {
        return status != null && status.startsWith("0");
    }

    /** Status key 1 = '2': the INVALID KEY condition. */
    public static boolean isInvalidKey(String status) {
        return status != null && status.startsWith("2");
    }
}
