package com.carddemo.programs.cobil00c.copybook;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * Signed zoned-decimal (PIC S9(n)V9(m) DISPLAY) codec using the ASCII overpunch convention of
 * the CardDemo ASCII data files: '{','A'..'I' = +0..+9 and '}','J'..'R' = -0..-9 in the last byte.
 */
public final class ZonedDecimal {

    private static final String POSITIVE = "{ABCDEFGHI";
    private static final String NEGATIVE = "}JKLMNOPQR";

    private ZonedDecimal() {
    }

    public static BigDecimal decode(String field, int scale) {
        if (field.isEmpty()) {
            throw new IllegalArgumentException("empty zoned field");
        }
        char last = field.charAt(field.length() - 1);
        boolean negative = false;
        int lastDigit;
        if (POSITIVE.indexOf(last) >= 0) {
            lastDigit = POSITIVE.indexOf(last);
        } else if (NEGATIVE.indexOf(last) >= 0) {
            lastDigit = NEGATIVE.indexOf(last);
            negative = true;
        } else if (Character.isDigit(last)) {
            lastDigit = last - '0';
        } else {
            throw new IllegalArgumentException("invalid zoned sign byte '" + last + "' in '" + field + "'");
        }
        String head = field.substring(0, field.length() - 1);
        for (int i = 0; i < head.length(); i++) {
            if (!Character.isDigit(head.charAt(i))) {
                throw new IllegalArgumentException("non-numeric zoned field '" + field + "'");
            }
        }
        BigInteger unscaled = new BigInteger(head.isEmpty() ? String.valueOf(lastDigit) : head + lastDigit);
        if (negative) {
            unscaled = unscaled.negate();
        }
        return new BigDecimal(unscaled, scale);
    }

    public static String encode(BigDecimal value, int totalDigits, int scale) {
        BigDecimal v = Cobol.toPic(value, totalDigits - scale, scale);
        String digits = v.abs().unscaledValue().toString();
        digits = "0".repeat(totalDigits - digits.length()) + digits;
        int last = digits.charAt(totalDigits - 1) - '0';
        char sign = v.signum() < 0 ? NEGATIVE.charAt(last) : POSITIVE.charAt(last);
        return digits.substring(0, totalDigits - 1) + sign;
    }
}
