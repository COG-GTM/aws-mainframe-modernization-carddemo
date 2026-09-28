package com.carddemo.batch.record;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

/**
 * Zoned-decimal (COBOL {@code DISPLAY}) numbers as they appear in the ASCII CardDemo files: digits with the
 * sign overpunched on the last byte ({@code {}, {@code A}-{@code I} positive, {@code }}, {@code J}-{@code R}
 * negative).
 */
public final class Zoned {

    private static final String POSITIVE = "{ABCDEFGHI";
    private static final String NEGATIVE = "}JKLMNOPQR";

    private Zoned() {
    }

    /** Parses a signed field {@code PIC S9(p-s)V9(s)}. Blank fields are zero. */
    public static BigDecimal parseSigned(String field, int scale) {
        String f = field.trim();
        if (f.isEmpty()) {
            return BigDecimal.ZERO.setScale(scale);
        }
        char last = f.charAt(f.length() - 1);
        String body = f.substring(0, f.length() - 1);
        boolean negative = false;
        int digit;
        if (Character.isDigit(last)) {
            digit = last - '0';
        } else if (POSITIVE.indexOf(last) >= 0) {
            digit = POSITIVE.indexOf(last);
        } else if (NEGATIVE.indexOf(last) >= 0) {
            digit = NEGATIVE.indexOf(last);
            negative = true;
        } else {
            throw new IllegalArgumentException("Invalid zoned decimal: '" + field + "'");
        }
        String digits = body + digit;
        requireDigits(digits, field);
        BigDecimal value = new BigDecimal(new BigInteger(digits), scale);
        return negative ? value.negate() : value;
    }

    /** Parses an unsigned field {@code PIC 9(n)}. */
    public static long parseUnsigned(String field) {
        String f = field.trim();
        if (f.isEmpty()) {
            return 0L;
        }
        requireDigits(f, field);
        return Long.parseLong(f);
    }

    /**
     * Formats a signed field {@code PIC S9(intDigits)V9(scale)} with overpunched sign, applying COBOL MOVE
     * semantics (fraction truncated, high-order digits dropped).
     */
    public static String formatSigned(BigDecimal value, int intDigits, int scale) {
        BigDecimal v = Cobol.fit(value, intDigits, scale);
        String digits = v.abs().unscaledValue().toString();
        int len = intDigits + scale;
        digits = "0".repeat(Math.max(0, len - digits.length())) + digits;
        int lastDigit = digits.charAt(len - 1) - '0';
        char sign = v.signum() < 0 ? NEGATIVE.charAt(lastDigit) : POSITIVE.charAt(lastDigit);
        return digits.substring(0, len - 1) + sign;
    }

    /** Formats an unsigned field {@code PIC 9(n)} (high-order truncation as in a COBOL MOVE). */
    public static String formatUnsigned(long value, int digits) {
        String s = Long.toString(Math.abs(value));
        if (s.length() > digits) {
            s = s.substring(s.length() - digits);
        }
        return "0".repeat(digits - s.length()) + s;
    }

    /** Converts to the given scale by truncation. */
    public static BigDecimal truncate(BigDecimal v, int scale) {
        return v.setScale(scale, RoundingMode.DOWN);
    }

    private static void requireDigits(String digits, String field) {
        for (int i = 0; i < digits.length(); i++) {
            if (!Character.isDigit(digits.charAt(i))) {
                throw new IllegalArgumentException("Invalid numeric field: '" + field + "'");
            }
        }
    }
}
