package com.carddemo.common.error;

import java.util.Arrays;
import java.util.Objects;

/**
 * A two-byte COBOL {@code FILE STATUS}. Only the statuses a caller explicitly expects are normal
 * outcomes; {@link #check} turns every other status into a {@link FileStatusException} (an abend).
 */
public record FileStatus(String code) {

    public static final FileStatus OK = new FileStatus("00");
    public static final FileStatus END_OF_FILE = new FileStatus("10");
    public static final FileStatus DUPLICATE_KEY = new FileStatus("22");
    public static final FileStatus RECORD_NOT_FOUND = new FileStatus("23");
    public static final FileStatus BOUNDARY_VIOLATION = new FileStatus("34");
    /** Record length does not conform to the file's fixed length (successful-with-condition in COBOL; an abend here). */
    public static final FileStatus RECORD_LENGTH_CONFLICT = new FileStatus("04");
    public static final FileStatus PERMANENT_ERROR = new FileStatus("30");
    public static final FileStatus FILE_NOT_FOUND = new FileStatus("35");
    public static final FileStatus OPEN_MODE_CONFLICT = new FileStatus("37");
    public static final FileStatus WRITE_LENGTH_ERROR = new FileStatus("44");

    public FileStatus {
        Objects.requireNonNull(code, "code");
        if (code.length() != 2) {
            throw new IllegalArgumentException("FILE STATUS is two bytes: '" + code + "'");
        }
    }

    public static FileStatus of(String code) {
        return new FileStatus(code);
    }

    public boolean isOk() {
        return OK.equals(this);
    }

    /**
     * Throws the typed exception for {@code status} unless it is one of {@code expected}
     * ({@code 00} is always expected).
     */
    public static void check(FileStatus status, String ddName, String operation, FileStatus... expected) {
        if (status.isOk() || Arrays.asList(expected).contains(status)) {
            return;
        }
        throw FileStatusException.of(status, ddName, operation);
    }

    /**
     * The four characters CBTRN02C {@code 9910-DISPLAY-IO-STATUS} shows after {@code 'FILE STATUS IS: NNNN'}:
     * {@code 00} + status for numeric statuses, else the first byte followed by the second byte's
     * binary value as three digits (e.g. {@code 9} + X'23' = {@code 9035}).
     */
    public String ioStatus04() {
        char stat1 = code.charAt(0);
        char stat2 = code.charAt(1);
        boolean numeric = Character.isDigit(stat1) && Character.isDigit(stat2);
        if (!numeric || stat1 == '9') {
            return stat1 + String.format("%03d", (int) stat2 % 1000);
        }
        return "00" + code;
    }

    /** The whole {@code 9910-DISPLAY-IO-STATUS} SYSOUT line. */
    public String displayLine() {
        return "FILE STATUS IS: NNNN" + ioStatus04();
    }

    @Override
    public String toString() {
        return code;
    }
}
