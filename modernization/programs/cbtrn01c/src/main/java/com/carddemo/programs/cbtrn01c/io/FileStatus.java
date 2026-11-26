package com.carddemo.programs.cbtrn01c.io;

/** Two-byte COBOL FILE STATUS values used by the record readers. */
public final class FileStatus {

    public static final String OK = "00";
    public static final String END_OF_FILE = "10";
    public static final String RECORD_NOT_FOUND = "23";
    public static final String PERMANENT_ERROR = "30";
    public static final String FILE_NOT_FOUND = "35";
    public static final String CLOSE_NOT_OPEN = "42";
    public static final String READ_NOT_OPEN = "47";
    public static final String WRONG_LENGTH = "04";

    private FileStatus() {
    }

    /** Status key 1 = '0': successful completion. */
    public static boolean isSuccessful(String status) {
        return status != null && !status.isEmpty() && status.charAt(0) == '0';
    }

    /** Status key 1 = '2': INVALID KEY condition. */
    public static boolean isInvalidKey(String status) {
        return status != null && !status.isEmpty() && status.charAt(0) == '2';
    }
}
