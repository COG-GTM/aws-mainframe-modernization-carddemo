package com.carddemo.programs.coactupc.support;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

/**
 * Codec for signed zoned-decimal DISPLAY items (e.g. {@code PIC S9(10)V99}) using the
 * ASCII trailing overpunch convention used by the CardDemo data files:
 * positive last digit 0-9 = '{','A'..'I'; negative = '}','J'..'R'.
 */
public final class ZonedDecimal {

    private static final String POSITIVE = "{ABCDEFGHI";
    private static final String NEGATIVE = "}JKLMNOPQR";

    private ZonedDecimal() {
    }

    /**
     * Stores {@code value} into a {@code PIC S9(intDigits)V9(scale)} item as COMPUTE / MOVE do
     * without ROUNDED: excess decimals are truncated and high-order integer digits are lost.
     */
    public static String encode(BigDecimal value, int intDigits, int scale) {
        BigDecimal truncated = truncate(value, intDigits, scale);
        boolean negative = truncated.signum() < 0;
        String digits = truncated.abs().unscaledValue().toString();
        int length = intDigits + scale;
        digits = "0".repeat(length - digits.length()) + digits;
        int last = digits.charAt(length - 1) - '0';
        char sign = negative ? NEGATIVE.charAt(last) : POSITIVE.charAt(last);
        return digits.substring(0, length - 1) + sign;
    }

    /** Decimal truncation semantics of a numeric receiving item. */
    public static BigDecimal truncate(BigDecimal value, int intDigits, int scale) {
        BigDecimal scaled = value.setScale(scale, RoundingMode.DOWN);
        BigInteger unscaled = scaled.unscaledValue();
        BigInteger modulus = BigInteger.TEN.pow(intDigits + scale);
        BigInteger kept = unscaled.abs().mod(modulus);
        if (unscaled.signum() < 0) {
            kept = kept.negate();
        }
        return new BigDecimal(kept, scale);
    }

    /** Reads a signed zoned item; non-digit bytes contribute their low-order nibble. */
    public static BigDecimal decode(String text, int scale) {
        BigInteger unscaled = BigInteger.ZERO;
        boolean negative = false;
        for (int i = 0; i < text.length(); i++) {
            char c = text.charAt(i);
            if (i == text.length() - 1 && (c == '}' || (c >= 'J' && c <= 'R'))) {
                negative = true;
            }
            unscaled = unscaled.multiply(BigInteger.TEN).add(BigInteger.valueOf(Cobol.zonedDigit(c)));
        }
        if (negative) {
            unscaled = unscaled.negate();
        }
        return new BigDecimal(unscaled, scale);
    }
}
