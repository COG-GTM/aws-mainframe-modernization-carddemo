package com.carddemo.common.batch;

import java.util.Objects;

/**
 * A primary account number (card number, e.g. DALYTRAN-CARD-NUM {@code PIC X(16)}) that never renders
 * in full: {@link #toString()} shows only the last 4 characters. Pass a {@code Pan} instead of the raw
 * string to {@link SysoutWriter#display} or to a logger.
 */
public final class Pan {

    private final String value;

    private Pan(String value) {
        this.value = Objects.requireNonNull(value, "value");
    }

    public static Pan of(String value) {
        return new Pan(value);
    }

    /** Masks all but the last 4 non-blank characters; values of 4 or fewer characters are fully masked. */
    public static String mask(String value) {
        if (value == null) {
            return null;
        }
        String trimmed = value.strip();
        int visible = trimmed.length() > 4 ? 4 : 0;
        return "*".repeat(trimmed.length() - visible) + trimmed.substring(trimmed.length() - visible);
    }

    /** The full number, for business logic only (lookups, record images); never for output. */
    public String value() {
        return value;
    }

    @Override
    public String toString() {
        return mask(value);
    }

    @Override
    public boolean equals(Object o) {
        return o instanceof Pan other && value.equals(other.value);
    }

    @Override
    public int hashCode() {
        return value.hashCode();
    }
}
