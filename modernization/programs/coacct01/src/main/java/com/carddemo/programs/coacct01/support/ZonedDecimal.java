package com.carddemo.programs.coacct01.support;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

/**
 * Signed zoned-decimal DISPLAY fields (PIC S9(n)V9(s), trailing overpunched sign) as they appear
 * in the CardDemo ASCII data files: '{' and 'A'..'I' are +0..+9, '}' and 'J'..'R' are -0..-9.
 */
public final class ZonedDecimal {

    private static final String POSITIVE = "{ABCDEFGHI";
    private static final String NEGATIVE = "}JKLMNOPQR";

    private ZonedDecimal() {
    }

    /** COBOL MOVE into PIC S9(intDigits)V9(scale): decimal truncation and high-order truncation. */
    public static BigDecimal truncate(BigDecimal value, int intDigits, int scale) {
        BigDecimal v = (value == null ? BigDecimal.ZERO : value).setScale(scale, RoundingMode.DOWN);
        BigInteger unscaled = v.unscaledValue();
        BigInteger mod = BigInteger.TEN.pow(intDigits + scale);
        BigInteger magnitude = unscaled.abs().mod(mod);
        return new BigDecimal(unscaled.signum() < 0 ? magnitude.negate() : magnitude, scale);
    }

    public static String format(BigDecimal value, int intDigits, int scale) {
        BigDecimal v = truncate(value, intDigits, scale);
        int width = intDigits + scale;
        String digits = v.unscaledValue().abs().toString();
        digits = "0".repeat(width - digits.length()) + digits;
        int last = digits.charAt(width - 1) - '0';
        char sign = v.signum() < 0 ? NEGATIVE.charAt(last) : POSITIVE.charAt(last);
        return digits.substring(0, width - 1) + sign;
    }

    public static BigDecimal parse(String text, int scale) {
        int width = text.length();
        char lastChar = text.charAt(width - 1);
        int last;
        boolean negative = false;
        if (POSITIVE.indexOf(lastChar) >= 0) {
            last = POSITIVE.indexOf(lastChar);
        } else if (NEGATIVE.indexOf(lastChar) >= 0) {
            last = NEGATIVE.indexOf(lastChar);
            negative = true;
        } else if (Character.isDigit(lastChar)) {
            last = lastChar - '0';
        } else {
            throw new NumberFormatException("Invalid zoned decimal sign in '" + text + "'");
        }
        String head = text.substring(0, width - 1);
        if (!head.chars().allMatch(Character::isDigit)) {
            throw new NumberFormatException("Invalid zoned decimal digits in '" + text + "'");
        }
        BigInteger unscaled = new BigInteger(head + last);
        return new BigDecimal(negative ? unscaled.negate() : unscaled, scale);
    }
}
