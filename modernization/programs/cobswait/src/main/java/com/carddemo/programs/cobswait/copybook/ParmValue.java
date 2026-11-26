package com.carddemo.programs.cobswait.copybook;

import java.util.Objects;

/**
 * {@code 01 PARM-VALUE PIC X(8).} (COBSWAIT WORKING-STORAGE).
 *
 * <p>Alphanumeric MOVE semantics: longer values are truncated on the right, shorter values are
 * padded on the right with spaces, so {@link #value()} is always exactly {@value #LENGTH} chars.
 */
public record ParmValue(String value) {

    public static final int LENGTH = 8;

    /** Initial WORKING-STORAGE content (no VALUE clause; GnuCOBOL/IBM default for PIC X is spaces). */
    public static final ParmValue SPACES = of("");

    public ParmValue {
        Objects.requireNonNull(value, "value");
        if (value.length() != LENGTH) {
            throw new IllegalArgumentException("PARM-VALUE must be exactly " + LENGTH + " chars");
        }
    }

    /** MOVE source TO PARM-VALUE: truncate or space-pad on the right to PIC X(8). */
    public static ParmValue of(String source) {
        String s = source == null ? "" : source;
        if (s.length() >= LENGTH) {
            return new ParmValue(s.substring(0, LENGTH));
        }
        return new ParmValue(s + " ".repeat(LENGTH - s.length()));
    }
}
