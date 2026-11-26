package com.carddemo.programs.paudbunl.io;

/** COBOL FILE STATUS values produced by the record readers/writers. */
public final class FileStatus {
    public static final String SUCCESS = "00";
    public static final String END_OF_FILE = "10";
    public static final String PERMANENT_IO_ERROR = "30";
    public static final String FILE_NOT_FOUND = "35";
    public static final String RECORD_LENGTH_CONFLICT = "44";
    public static final String NOT_OPEN = "42";
    public static final String ALREADY_OPEN = "41";
    public static final String WRITE_NOT_OPEN = "48";
    public static final String READ_NOT_OPEN = "47";

    private FileStatus() {
    }

    /** {@code status = SPACES OR '00'} as tested by PAUDBUNL after OPEN/CLOSE. */
    public static boolean isOk(String status) {
        return status == null || status.isBlank() || SUCCESS.equals(status);
    }
}
