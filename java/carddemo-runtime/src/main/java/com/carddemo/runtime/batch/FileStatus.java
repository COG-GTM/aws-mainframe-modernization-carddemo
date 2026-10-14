package com.carddemo.runtime.batch;

/** COBOL FILE STATUS codes (2 characters) used by CardDemo batch programs. */
public enum FileStatus {
    SUCCESS("00"), DUPLICATE_ALT_KEY("02"), LENGTH_MISMATCH("04"), OPTIONAL_NOT_PRESENT("05"),
    END_OF_FILE("10"), SEQUENCE_ERROR("21"), DUPLICATE_KEY("22"), RECORD_NOT_FOUND("23"),
    BOUNDARY_VIOLATION("24"), PERMANENT_ERROR("30"), FILE_NOT_FOUND("35"), OPEN_MODE_CONFLICT("37"),
    ALREADY_OPEN("41"), NOT_OPEN("42"), NO_PRIOR_READ("43"), RECORD_LENGTH_ERROR("44"),
    READ_AFTER_EOF("46"), NOT_OPEN_INPUT("47"), NOT_OPEN_OUTPUT("48"), NOT_OPEN_IO("49");

    private final String code;

    FileStatus(String code) {
        this.code = code;
    }

    public String code() {
        return code;
    }

    /** Status key 1 = '0': successful completion. */
    public boolean successful() {
        return code.charAt(0) == '0';
    }

    public static FileStatus of(String code) {
        for (FileStatus s : values()) {
            if (s.code.equals(code)) {
                return s;
            }
        }
        throw new IllegalArgumentException("Unknown file status " + code);
    }
}
