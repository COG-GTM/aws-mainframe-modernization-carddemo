package com.carddemo.programs.cotrn00c.copybook;

import java.math.BigDecimal;
import java.math.BigInteger;

/** Signed zoned-decimal (PIC S9(n)V9(m) DISPLAY, trailing overpunched sign) codec. */
public final class ZonedDecimal {

    private static final String POSITIVE = "{ABCDEFGHI";
    private static final String NEGATIVE = "}JKLMNOPQR";
    private static final String NEGATIVE_ASCII = "pqrstuvwxy";

    private ZonedDecimal() {
    }

    public static BigDecimal parse(String field, int scale) {
        if (field == null || field.isEmpty()) {
            throw new IllegalArgumentException("empty zoned decimal");
        }
        char last = field.charAt(field.length() - 1);
        int digit;
        boolean negative;
        if (last >= '0' && last <= '9') {
            digit = last - '0';
            negative = false;
        } else if (POSITIVE.indexOf(last) >= 0) {
            digit = POSITIVE.indexOf(last);
            negative = false;
        } else if (NEGATIVE.indexOf(last) >= 0) {
            digit = NEGATIVE.indexOf(last);
            negative = true;
        } else if (NEGATIVE_ASCII.indexOf(last) >= 0) {
            digit = NEGATIVE_ASCII.indexOf(last);
            negative = true;
        } else {
            throw new IllegalArgumentException("invalid zoned sign byte '" + last + "' in " + field);
        }
        String head = field.substring(0, field.length() - 1);
        if (!head.isEmpty() && !CobolText.isNumeric(head)) {
            throw new IllegalArgumentException("non-numeric zoned decimal: " + field);
        }
        BigInteger unscaled = new BigInteger(head + digit);
        if (negative) {
            unscaled = unscaled.negate();
        }
        return new BigDecimal(unscaled, scale);
    }

    /** Format with high-order truncation to {@code length} digits and an overpunched sign. */
    public static String format(BigDecimal value, int length, int scale) {
        BigInteger unscaled = value.setScale(scale, java.math.RoundingMode.DOWN).unscaledValue();
        boolean negative = unscaled.signum() < 0;
        String digits = unscaled.abs().mod(BigInteger.TEN.pow(length)).toString();
        digits = "0".repeat(length - digits.length()) + digits;
        int last = digits.charAt(length - 1) - '0';
        char sign = negative ? NEGATIVE.charAt(last) : POSITIVE.charAt(last);
        return digits.substring(0, length - 1) + sign;
    }
}
