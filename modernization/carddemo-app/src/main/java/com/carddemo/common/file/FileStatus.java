package com.carddemo.common.file;

import java.util.Arrays;

/** COBOL {@code FILE STATUS} codes (two-character status keys) used by the CardDemo programs. */
public enum FileStatus {
    SUCCESS("00"),
    DUPLICATE_ALTERNATE_KEY("02"),
    RECORD_LENGTH_MISMATCH("04"),
    OPTIONAL_FILE_MISSING("05"),
    NOT_A_REEL("07"),
    END_OF_FILE("10"),
    RELATIVE_KEY_OVERFLOW("14"),
    SEQUENCE_ERROR("21"),
    DUPLICATE_KEY("22"),
    RECORD_NOT_FOUND("23"),
    BOUNDARY_VIOLATION("24"),
    PERMANENT_ERROR("30"),
    SEQUENTIAL_BOUNDARY_VIOLATION("34"),
    FILE_NOT_FOUND("35"),
    OPEN_MODE_NOT_ALLOWED("37"),
    CLOSED_WITH_LOCK("38"),
    ATTRIBUTE_MISMATCH("39"),
    ALREADY_OPEN("41"),
    NOT_OPEN("42"),
    NO_PRIOR_READ("43"),
    RECORD_LENGTH_ERROR("44"),
    NO_NEXT_RECORD("46"),
    NOT_OPEN_INPUT("47"),
    NOT_OPEN_OUTPUT("48"),
    NOT_OPEN_IO("49"),
    VSAM_OTHER_ERROR("90"),
    VSAM_PASSWORD_FAILURE("91"),
    VSAM_LOGIC_ERROR("92"),
    VSAM_RESOURCE_UNAVAILABLE("93"),
    VSAM_NO_CURRENT_RECORD("94"),
    VSAM_INVALID_FILE_INFO("95"),
    VSAM_NO_DD("96"),
    VSAM_OPEN_INTEGRITY_VERIFIED("97");

    private final String code;

    FileStatus(String code) {
        this.code = code;
    }

    public static FileStatus of(String code) {
        return Arrays.stream(values()).filter(s -> s.code.equals(code)).findFirst()
                .orElseThrow(() -> new IllegalArgumentException("unknown FILE STATUS '" + code + "'"));
    }

    public String code() {
        return code;
    }

    /** Status key 1 is {@code 0}: the operation succeeded. */
    public boolean isSuccessful() {
        return code.charAt(0) == '0';
    }

    public boolean isEndOfFile() {
        return this == END_OF_FILE;
    }

    /** Status key 1 is {@code 2}. */
    public boolean isInvalidKey() {
        return code.charAt(0) == '2';
    }

    /** Status key 1 is {@code 3}. */
    public boolean isPermanentError() {
        return code.charAt(0) == '3';
    }

    /** Status key 1 is {@code 4}. */
    public boolean isLogicError() {
        return code.charAt(0) == '4';
    }

    /** Status key 1 is {@code 9}. */
    public boolean isImplementorDefined() {
        return code.charAt(0) == '9';
    }

    /**
     * The batch programs' {@code 9910-DISPLAY-IO-STATUS} line for a raw two-character status: {@code NNNN00xx}
     * for a numeric status, otherwise status key 1 followed by the binary value of status key 2 as three digits
     * (GnuCOBOL's {@code 9x} statuses, e.g. {@code NNNN9035}).
     */
    public static String displayIoStatus(String rawStatus) {
        if (rawStatus.length() != 2) {
            throw new IllegalArgumentException("FILE STATUS is two characters, got '" + rawStatus + "'");
        }
        char key1 = rawStatus.charAt(0);
        char key2 = rawStatus.charAt(1);
        boolean numeric = Character.isDigit(key1) && Character.isDigit(key2);
        String status = numeric && key1 != '9'
                ? "00" + rawStatus
                : key1 + String.format("%03d", key2 & 0xFF);
        return "FILE STATUS IS: NNNN" + status;
    }

    public String displayIoStatus() {
        return displayIoStatus(code);
    }

    @Override
    public String toString() {
        return code + " " + name();
    }
}
