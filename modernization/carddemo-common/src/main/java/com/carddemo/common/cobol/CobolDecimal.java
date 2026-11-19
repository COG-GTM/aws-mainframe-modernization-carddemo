package com.carddemo.common.cobol;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Objects;

/**
 * Arithmetic storage rules of a COBOL numeric receiving field {@code PIC S9(integerDigits)V9(scale)}.
 * Without {@code ROUNDED}, excess decimals are dropped toward zero; with {@code ROUNDED}, they round
 * half away from zero. Without {@code ON SIZE ERROR}, excess high-order integer digits are dropped
 * silently (e.g. CBTRN02C {@code WS-TEMP-BAL PIC S9(09)V99}, D-014). Money is always {@link BigDecimal};
 * no method accepts {@code double} or {@code float}.
 */
public final class CobolDecimal {

    private CobolDecimal() {
    }

    /** Drops decimals beyond {@code scale} toward zero (MOVE / ADD / COMPUTE without ROUNDED). */
    public static BigDecimal truncate(BigDecimal value, int scale) {
        return Objects.requireNonNull(value, "value").setScale(scale, RoundingMode.DOWN);
    }

    /** {@code ROUNDED}: half away from zero. */
    public static BigDecimal rounded(BigDecimal value, int scale) {
        return Objects.requireNonNull(value, "value").setScale(scale, RoundingMode.HALF_UP);
    }

    /** {@code COMPUTE target = dividend / divisor} without ROUNDED into a field with {@code scale} decimals. */
    public static BigDecimal divide(BigDecimal dividend, BigDecimal divisor, int scale) {
        return dividend.divide(divisor, scale, RoundingMode.DOWN);
    }

    /** Stores {@code value} into {@code PIC S9(integerDigits)V9(scale)} without ROUNDED and without ON SIZE ERROR. */
    public static BigDecimal store(BigDecimal value, int integerDigits, int scale) {
        return dropHighOrder(truncate(value, scale), integerDigits);
    }

    /** As {@link #store} with {@code ROUNDED}. */
    public static BigDecimal storeRounded(BigDecimal value, int integerDigits, int scale) {
        return dropHighOrder(rounded(value, scale), integerDigits);
    }

    /** Stores into an unsigned {@code PIC 9(integerDigits)V9(scale)}: the sign is lost, as in COBOL. */
    public static BigDecimal storeUnsigned(BigDecimal value, int integerDigits, int scale) {
        return store(value, integerDigits, scale).abs();
    }

    /** True when {@code value} fits the picture without losing any digit. */
    public static boolean fits(BigDecimal value, int integerDigits, int scale) {
        if (value.signum() == 0) {
            return true;
        }
        BigDecimal stripped = value.stripTrailingZeros();
        int fraction = Math.max(stripped.scale(), 0);
        int integer = stripped.precision() - stripped.scale();
        return fraction <= scale && integer <= integerDigits;
    }

    private static BigDecimal dropHighOrder(BigDecimal value, int integerDigits) {
        if (integerDigits < 0) {
            throw new IllegalArgumentException("integerDigits must be >= 0: " + integerDigits);
        }
        BigDecimal modulus = BigDecimal.TEN.pow(integerDigits);
        BigDecimal kept = value.abs().remainder(modulus);
        return value.signum() < 0 ? kept.negate() : kept;
    }
}
