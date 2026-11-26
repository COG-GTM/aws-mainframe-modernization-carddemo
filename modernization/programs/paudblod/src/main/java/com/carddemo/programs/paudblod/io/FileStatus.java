package com.carddemo.programs.paudblod.io;

/** COBOL two-byte FILE STATUS values used by the sequential readers. */
public final class FileStatus {
    public static final String SUCCESS = "00";
    public static final String RECORD_LENGTH_MISMATCH = "04";
    public static final String END_OF_FILE = "10";
    public static final String PERMANENT_ERROR = "30";
    public static final String FILE_NOT_FOUND = "35";
    public static final String NOT_OPEN = "47";

    private FileStatus() {
    }

    /** {@code status = SPACES OR '00'}. */
    public static boolean isOk(String status) {
        return status == null || status.isBlank() || SUCCESS.equals(status);
    }
}
