package com.carddemo.interest.io;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * COBOL fixed-point arithmetic semantics expressed with {@link BigDecimal}.
 *
 * <p>COBOL numerics are fixed-point decimals, never binary floating point, which is why the Java
 * port uses {@link BigDecimal} throughout. Two COBOL behaviours have to be reproduced explicitly:
 *
 * <ul>
 *   <li><b>Low-order truncation.</b> A {@code COMPUTE} without the {@code ROUNDED} phrase discards
 *       the excess fraction digits — it truncates toward zero, it does not round half-up.</li>
 *   <li><b>High-order truncation.</b> Storing a value that is too large for the receiving PIC
 *       clause, with no {@code ON SIZE ERROR} phrase, silently drops the leading digits.</li>
 * </ul>
 */
public final class CobolFixedPoint {

    private CobolFixedPoint() {
    }

    /**
     * Truncates {@code value} toward zero to {@code scale} fraction digits, reproducing an
     * unrounded COBOL {@code COMPUTE} / {@code MOVE} into a {@code V9(scale)} receiving field.
     */
    public static BigDecimal truncate(BigDecimal value, int scale) {
        return value.setScale(scale, RoundingMode.DOWN);
    }

    /**
     * Fits {@code value} into a {@code PIC S9(integerDigits)V9(scale)} receiving field: fraction
     * digits are truncated toward zero and leading digits beyond the field capacity are dropped.
     */
    public static BigDecimal fit(BigDecimal value, int integerDigits, int scale) {
        BigDecimal truncated = truncate(value, scale);
        BigDecimal modulus = BigDecimal.TEN.pow(integerDigits);
        BigDecimal magnitude = truncated.abs();
        if (magnitude.compareTo(modulus) >= 0) {
            magnitude = magnitude.remainder(modulus);
            return truncated.signum() < 0 ? magnitude.negate() : magnitude;
        }
        return truncated;
    }
}
