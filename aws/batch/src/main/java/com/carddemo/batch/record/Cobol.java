package com.carddemo.batch.record;

import java.math.BigDecimal;
import java.math.RoundingMode;

/** COBOL arithmetic helpers (conventions.md: truncation unless the source says {@code ROUNDED}). */
public final class Cobol {

    private Cobol() {
    }

    /**
     * Stores {@code value} into a {@code PIC S9(intDigits)V9(scale)} receiving field without {@code ROUNDED} or
     * {@code ON SIZE ERROR}: excess fraction digits are truncated and excess high-order integer digits are lost.
     */
    public static BigDecimal fit(BigDecimal value, int intDigits, int scale) {
        BigDecimal v = value.setScale(scale, RoundingMode.DOWN);
        BigDecimal modulus = BigDecimal.TEN.pow(intDigits);
        BigDecimal intPart = v.abs().setScale(0, RoundingMode.DOWN);
        if (intPart.compareTo(modulus) >= 0) {
            BigDecimal frac = v.abs().subtract(intPart);
            BigDecimal kept = intPart.remainder(modulus).add(frac);
            v = v.signum() < 0 ? kept.negate() : kept;
        }
        return v.setScale(scale, RoundingMode.DOWN);
    }
}
