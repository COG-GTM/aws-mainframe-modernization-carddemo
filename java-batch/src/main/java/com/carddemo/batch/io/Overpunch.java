package com.carddemo.batch.io;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * Zoned-decimal sign overpunch for {@code PIC S9(n)V9(m)} display fields as stored in
 * {@code app/data/ASCII}: the last digit carries the sign, {@code {}=+0, A-I=+1..9,
 * }=-0, J-R=-1..9}.
 */
public final class Overpunch {

    private static final String POSITIVE = "{ABCDEFGHI";
    private static final String NEGATIVE = "}JKLMNOPQR";

    private Overpunch() {
    }

    /** Parses {@code text} (integer + fraction digits, sign overpunched on the last one) at {@code scale}. */
    public static BigDecimal parse(String text, int scale) {
        if (text.isEmpty()) {
            throw new IllegalArgumentException("empty signed field");
        }
        char last = text.charAt(text.length() - 1);
        int digit;
        boolean negative;
        if ((digit = POSITIVE.indexOf(last)) >= 0) {
            negative = false;
        } else if ((digit = NEGATIVE.indexOf(last)) >= 0) {
            negative = true;
        } else {
            throw new IllegalArgumentException("invalid overpunch sign '" + last + "' in '" + text + "'");
        }
        String digits = text.substring(0, text.length() - 1) + digit;
        for (int i = 0; i < digits.length(); i++) {
            if (!Character.isDigit(digits.charAt(i))) {
                throw new IllegalArgumentException("non-digit in signed field '" + text + "'");
            }
        }
        BigInteger unscaled = new BigInteger(digits);
        return new BigDecimal(negative ? unscaled.negate() : unscaled, scale);
    }

    /**
     * Formats {@code value} as {@code length} zoned digits at {@code scale} with the sign overpunched
     * on the last digit. Zero is written as positive ({@code {}). Throws if the value does not fit.
     */
    public static String format(BigDecimal value, int length, int scale) {
        BigDecimal scaled = value.setScale(scale, java.math.RoundingMode.UNNECESSARY);
        BigInteger unscaled = scaled.unscaledValue();
        boolean negative = unscaled.signum() < 0;
        String digits = unscaled.abs().toString();
        if (digits.length() > length) {
            throw new IllegalArgumentException(value + " does not fit in " + length + " digits");
        }
        String padded = "0".repeat(length - digits.length()) + digits;
        int lastDigit = padded.charAt(length - 1) - '0';
        char sign = (negative ? NEGATIVE : POSITIVE).charAt(lastDigit);
        return padded.substring(0, length - 1) + sign;
    }
}
