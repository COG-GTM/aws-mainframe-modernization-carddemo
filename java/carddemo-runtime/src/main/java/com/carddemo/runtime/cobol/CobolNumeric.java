package com.carddemo.runtime.cobol;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

/**
 * Numeric MOVE / COMPUTE store semantics for PIC 9/S9 items: the receiving item keeps
 * {@code integerDigits} digits before and {@code scale} digits after the implied decimal point.
 *
 * <ul>
 *   <li>Excess fractional digits are truncated (not rounded) unless ROUNDED is specified.</li>
 *   <li>Excess high-order integer digits are silently truncated (no ON SIZE ERROR handling).</li>
 *   <li>Unsigned receivers store the absolute value.</li>
 * </ul>
 */
public final class CobolNumeric {

    private CobolNumeric() {
    }

    /** MOVE / COMPUTE without ROUNDED. */
    public static BigDecimal store(BigDecimal value, int integerDigits, int scale, boolean signed) {
        return store(value, integerDigits, scale, signed, false);
    }

    /** COMPUTE ... ROUNDED: half away from zero (COBOL default rounding mode). */
    public static BigDecimal storeRounded(BigDecimal value, int integerDigits, int scale, boolean signed) {
        return store(value, integerDigits, scale, signed, true);
    }

    public static BigDecimal store(BigDecimal value, int integerDigits, int scale, boolean signed, boolean rounded) {
        BigDecimal v = value.setScale(scale, rounded ? RoundingMode.HALF_UP : RoundingMode.DOWN);
        BigInteger unscaled = v.unscaledValue();
        BigInteger modulus = BigInteger.TEN.pow(integerDigits + scale);
        BigInteger truncated = unscaled.abs().mod(modulus);
        if (signed && unscaled.signum() < 0) {
            truncated = truncated.negate();
        }
        return new BigDecimal(truncated, scale);
    }

    /** True when {@code value} does not fit the receiving picture (would raise ON SIZE ERROR). */
    public static boolean sizeError(BigDecimal value, int integerDigits, int scale) {
        BigDecimal limit = BigDecimal.TEN.pow(integerDigits);
        return value.setScale(scale, RoundingMode.DOWN).abs().compareTo(limit) >= 0;
    }
}
