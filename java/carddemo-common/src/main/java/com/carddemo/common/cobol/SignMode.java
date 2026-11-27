package com.carddemo.common.cobol;

/** Sign representation of a signed DISPLAY numeric (SIGN [IS] LEADING|TRAILING [SEPARATE CHARACTER]). */
public enum SignMode {
    /** IBM default for {@code PIC S9...}: sign carried in the zone of the last digit. */
    TRAILING_OVERPUNCH,
    LEADING_OVERPUNCH,
    TRAILING_SEPARATE,
    LEADING_SEPARATE;

    public boolean separate() {
        return this == TRAILING_SEPARATE || this == LEADING_SEPARATE;
    }

    public boolean leading() {
        return this == LEADING_OVERPUNCH || this == LEADING_SEPARATE;
    }
}
