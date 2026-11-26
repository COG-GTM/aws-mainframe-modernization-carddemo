package com.carddemo.programs.cbtrn03c.support;

import java.math.BigDecimal;
import java.math.RoundingMode;

/** Fixed-point store semantics for PIC S9(i)V9(s) without ON SIZE ERROR. */
public final class CobolNumeric {
    private CobolNumeric() {
    }

    /** Truncate low-order fraction digits and high-order integer digits (no rounding, sign kept). */
    public static BigDecimal truncate(BigDecimal value, int integerDigits, int scale) {
        BigDecimal v = value.setScale(scale, RoundingMode.DOWN);
        return v.remainder(BigDecimal.TEN.pow(integerDigits)).setScale(scale, RoundingMode.DOWN);
    }
}
