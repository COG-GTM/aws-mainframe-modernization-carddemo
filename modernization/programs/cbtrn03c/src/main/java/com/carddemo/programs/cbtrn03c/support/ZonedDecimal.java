package com.carddemo.programs.cbtrn03c.support;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

/**
 * Signed zoned-decimal (PIC S9(n)V9(m) DISPLAY, trailing overpunched sign) in its ASCII form:
 * '{' and 'A'-'I' are +0..+9, '}' and 'J'-'R' are -0..-9; a plain digit is treated as positive.
 */
public final class ZonedDecimal {
    private static final String POSITIVE = "{ABCDEFGHI";
    private static final String NEGATIVE = "}JKLMNOPQR";

    private ZonedDecimal() {
    }

    public static BigDecimal decode(String field, int scale) {
        if (field.isBlank()) {
            return BigDecimal.ZERO.setScale(scale);
        }
        String body = field.substring(0, field.length() - 1);
        char last = field.charAt(field.length() - 1);
        int digit;
        boolean negative = false;
        if (last >= '0' && last <= '9') {
            digit = last - '0';
        } else if (POSITIVE.indexOf(last) >= 0) {
            digit = POSITIVE.indexOf(last);
        } else if (NEGATIVE.indexOf(last) >= 0) {
            digit = NEGATIVE.indexOf(last);
            negative = true;
        } else {
            throw new IllegalArgumentException("Invalid zoned sign byte in '" + field + "'");
        }
        String digits = body.replace(' ', '0') + digit;
        if (!Fixed.isNumeric(digits)) {
            throw new IllegalArgumentException("Invalid zoned decimal '" + field + "'");
        }
        BigDecimal value = new BigDecimal(new BigInteger(digits), scale);
        return negative ? value.negate() : value;
    }

    /** Encode with the preferred sign (C for positive/zero, D for negative), as the compiler stores results. */
    public static String encode(BigDecimal value, int integerDigits, int scale) {
        BigDecimal v = CobolNumeric.truncate(value, integerDigits, scale);
        String digits = v.abs().movePointRight(scale).setScale(0, RoundingMode.DOWN).toBigInteger().toString();
        int length = integerDigits + scale;
        digits = "0".repeat(length - digits.length()) + digits;
        int last = digits.charAt(length - 1) - '0';
        char sign = v.signum() < 0 ? NEGATIVE.charAt(last) : POSITIVE.charAt(last);
        return digits.substring(0, length - 1) + sign;
    }
}
