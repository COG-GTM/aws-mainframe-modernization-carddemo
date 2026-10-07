package com.carddemo.common;

import java.util.Objects;

/**
 * Unrecoverable error that replaces {@code CALL 'CEE3ABD' USING ABCODE, TIMING} (ADR-0013).
 *
 * <p>The original abend code is kept so batch jobs can map it to the step's return code and logs read like the
 * mainframe job log ({@code USER ABEND U0999}).
 */
public class AbendException extends RuntimeException {

    /** Abend code used by every CardDemo batch program's {@code 9999-ABEND-PROGRAM} paragraph. */
    public static final int CARDDEMO_ABEND_CODE = 999;

    private final int abendCode;

    public AbendException(int abendCode, String message) {
        this(abendCode, message, null);
    }

    public AbendException(int abendCode, String message, Throwable cause) {
        super(format(abendCode, message), cause);
        if (abendCode < 0 || abendCode > 4095) {
            throw new IllegalArgumentException("User abend codes are 0..4095, got " + abendCode);
        }
        this.abendCode = abendCode;
    }

    /** {@code MOVE 999 TO ABCODE} + {@code CALL 'CEE3ABD'}, the only abend CardDemo issues. */
    public static AbendException carddemo(String message, Throwable cause) {
        return new AbendException(CARDDEMO_ABEND_CODE, message, cause);
    }

    public int abendCode() {
        return abendCode;
    }

    /** Mainframe-style abend label, e.g. {@code U0999}. */
    public String abendLabel() {
        return String.format(java.util.Locale.ROOT, "U%04d", abendCode);
    }

    private static String format(int abendCode, String message) {
        return String.format(java.util.Locale.ROOT, "USER ABEND U%04d: %s", abendCode, Objects.requireNonNullElse(message, ""));
    }
}
