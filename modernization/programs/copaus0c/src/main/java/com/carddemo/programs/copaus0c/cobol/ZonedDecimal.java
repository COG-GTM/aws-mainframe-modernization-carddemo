package com.carddemo.programs.copaus0c.cobol;

import java.math.BigDecimal;
import java.math.BigInteger;

/** Signed zoned-decimal (trailing overpunch sign) as found in the ASCII sample data files. */
public final class ZonedDecimal {

    private ZonedDecimal() {
    }

    public static BigDecimal parse(String text, int scale) {
        int last = text.length() - 1;
        char c = text.charAt(last);
        int digit;
        boolean negative = false;
        if (c >= '0' && c <= '9') {
            digit = c - '0';
        } else if (c == '{') {
            digit = 0;
        } else if (c >= 'A' && c <= 'I') {
            digit = c - 'A' + 1;
        } else if (c == '}') {
            digit = 0;
            negative = true;
        } else if (c >= 'J' && c <= 'R') {
            digit = c - 'J' + 1;
            negative = true;
        } else {
            throw new IllegalArgumentException("Invalid zoned sign character: " + c);
        }
        BigInteger unscaled = new BigInteger(text.substring(0, last) + digit);
        return new BigDecimal(negative ? unscaled.negate() : unscaled, scale);
    }
}
