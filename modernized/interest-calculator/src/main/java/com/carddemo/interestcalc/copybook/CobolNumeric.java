package com.carddemo.interestcalc.copybook;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

/**
 * COBOL fixed-point storage semantics for {@code PIC S9(i)V9(d)} items.
 *
 * <p>Two behaviours of a COBOL {@code MOVE}/{@code COMPUTE} into such an item are reproduced
 * here, both of which differ from what a naive Java translation would do:
 *
 * <ol>
 *   <li><b>Low-order truncation.</b> Without a {@code ROUNDED} phrase the result is truncated,
 *       not rounded, and COBOL truncation is toward zero. That is
 *       {@link RoundingMode#DOWN} - <em>not</em> {@link RoundingMode#HALF_UP} and
 *       <em>not</em> {@link RoundingMode#FLOOR} (which would differ for negative values).</li>
 *   <li><b>High-order truncation.</b> Without an {@code ON SIZE ERROR} phrase, digits that do
 *       not fit in the receiving field are discarded silently, keeping the sign.</li>
 * </ol>
 */
public final class CobolNumeric {

    private CobolNumeric() {
    }

    /** Stores {@code value} into a {@code PIC S9(intDigits)V9(decDigits)} item. */
    public static BigDecimal store(BigDecimal value, int intDigits, int decDigits) {
        BigDecimal truncated = value.setScale(decDigits, RoundingMode.DOWN);
        BigInteger modulus = BigInteger.TEN.pow(intDigits + decDigits);
        BigInteger unscaled = truncated.unscaledValue();
        BigInteger reduced = unscaled.abs().mod(modulus);
        if (unscaled.signum() < 0) {
            reduced = reduced.negate();
        }
        return new BigDecimal(reduced, decDigits);
    }
}
