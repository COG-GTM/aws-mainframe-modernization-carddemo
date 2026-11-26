package com.carddemo.programs.cbact01c.support;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.util.Locale;

/**
 * USAGE DISPLAY numerics ({@code PIC 9(n)} / {@code PIC S9(n)V9(m)}) with a trailing overpunched sign,
 * as found in the ASCII CardDemo data: '{' / 'A'-'I' = +0..+9, '}' / 'J'-'R' = -0..-9.
 * A plain digit in the sign position is treated as positive (unsigned / F zone).
 */
public final class ZonedDecimal {
    private static final String POSITIVE = "{ABCDEFGHI";
    private static final String NEGATIVE = "}JKLMNOPQR";

    private ZonedDecimal() {
    }

    public static BigDecimal decode(String field, int scale) {
        if (field.isEmpty()) {
            throw new DataException("empty zoned field");
        }
        StringBuilder digits = new StringBuilder(field.length());
        for (int i = 0; i < field.length() - 1; i++) {
            char c = field.charAt(i);
            if (c < '0' || c > '9') {
                throw new DataException("invalid zoned digit '" + c + "' in [" + field + "]");
            }
            digits.append(c);
        }
        char last = field.charAt(field.length() - 1);
        boolean negative;
        int p = POSITIVE.indexOf(last);
        int n = NEGATIVE.indexOf(last);
        if (last >= '0' && last <= '9') {
            digits.append(last);
            negative = false;
        } else if (p >= 0) {
            digits.append((char) ('0' + p));
            negative = false;
        } else if (n >= 0) {
            digits.append((char) ('0' + n));
            negative = true;
        } else {
            throw new DataException("invalid zoned sign '" + last + "' in [" + field + "]");
        }
        BigInteger unscaled = new BigInteger(digits.toString());
        return new BigDecimal(negative ? unscaled.negate() : unscaled, scale);
    }

    /**
     * MOVE of a numeric value to {@code PIC [S]9(intDigits)V9(scale)} DISPLAY: excess decimals are truncated,
     * excess high-order digits are dropped, signed fields get the preferred (C/D) overpunch.
     */
    public static String encode(BigDecimal value, int intDigits, int scale, boolean signed) {
        int total = intDigits + scale;
        BigInteger unscaled = value.setScale(scale, RoundingMode.DOWN).unscaledValue();
        boolean negative = signed && unscaled.signum() < 0;
        BigInteger magnitude = unscaled.abs().mod(BigInteger.TEN.pow(total));
        String digits = String.format(Locale.ROOT, "%0" + total + "d", magnitude);
        if (!signed) {
            return digits;
        }
        int lastDigit = digits.charAt(total - 1) - '0';
        char sign = negative ? NEGATIVE.charAt(lastDigit) : POSITIVE.charAt(lastDigit);
        return digits.substring(0, total - 1) + sign;
    }
}
