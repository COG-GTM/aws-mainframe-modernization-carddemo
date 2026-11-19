package com.carddemo.common.cobol;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * Arithmetic results stored the way COBOL stores them in a {@code PIC S9(i)V9(d)} receiving field when the statement has
 * neither {@code ROUNDED} nor {@code ON SIZE ERROR}: excess decimals are truncated and excess high-order integer digits are
 * dropped (the sign is kept).
 */
public final class CobolDecimal {

    private CobolDecimal() {
    }

    /** A stored result: its value and whether the receiving field holds zero with a negative sign. */
    public record Stored(BigDecimal value, boolean negativeZero) {
    }

    /**
     * {@link #store} plus the sign GnuCOBOL leaves in the field: the sign of the untruncated result. A result that is zero
     * before truncation is positive (even {@code 0 + -0}); a negative result whose remaining digits are all zero (e.g.
     * {@code -999999999.99 + -0.01} into {@code S9(09)V99}) is negative zero (D-052).
     */
    public static Stored storeWithSign(BigDecimal value, int integerDigits, int scale) {
        BigDecimal stored = store(value, integerDigits, scale);
        return new Stored(stored, value.signum() < 0 && stored.signum() == 0);
    }

    public static BigDecimal store(BigDecimal value, int integerDigits, int scale) {
        BigDecimal truncated = value.setScale(scale, RoundingMode.DOWN);
        return truncated.remainder(BigDecimal.TEN.pow(integerDigits)).setScale(scale, RoundingMode.UNNECESSARY);
    }
}
