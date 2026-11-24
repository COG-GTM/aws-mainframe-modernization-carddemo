package com.carddemo.transaction.cobol;

/**
 * Two-byte COBOL FILE STATUS codes used by the CardDemo batch programs.
 */
public enum FileStatus {
    SUCCESS("00"),
    END_OF_FILE("10"),
    DUPLICATE_KEY("22"),
    RECORD_NOT_FOUND("23"),
    PERMANENT_IO_ERROR("30"),
    FILE_NOT_FOUND("35"),
    OPEN_MODE_CONFLICT("37"),
    LOGIC_ERROR("92");

    private final String code;

    FileStatus(String code) {
        this.code = code;
    }

    public String code() {
        return code;
    }

    /** {@code '00'} - APPL-AOK. */
    public boolean isSuccess() {
        return this == SUCCESS;
    }

    /** {@code '10'} - APPL-EOF. */
    public boolean isEndOfFile() {
        return this == END_OF_FILE;
    }

    /**
     * Port of {@code Z-DISPLAY-IO-STATUS} / {@code 9910-DISPLAY-IO-STATUS}: renders the status as the
     * four character {@code IO-STATUS-04} value, e.g. {@code 0035}; for {@code 9x} statuses the second byte
     * is shown as its binary value ({@code 9nnn}).
     */
    public static String displayIoStatus(String ioStatus) {
        String status = ioStatus == null ? "  " : Pic.x(ioStatus, 2);
        if (!Pic.isNumeric(status) || status.charAt(0) == '9') {
            int binary = status.charAt(1) & 0xFF;
            return "FILE STATUS IS: NNNN" + status.charAt(0) + String.format("%03d", binary);
        }
        return "FILE STATUS IS: NNNN00" + status;
    }
}
