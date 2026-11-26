package com.carddemo.programs.cbact04c.copybook;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * COBOL USAGE DISPLAY field helpers: fixed-width encode/decode and MOVE/arithmetic truncation.
 * Signed zoned decimals use a trailing overpunch sign ('{'/'A'-'I' positive, '}'/'J'-'R' negative),
 * which is how the ASCII sample data in app/data/ASCII encodes S9(n)V99 fields.
 */
public final class Pic {

    private static final String POSITIVE_OVERPUNCH = "{ABCDEFGHI";
    private static final String NEGATIVE_OVERPUNCH = "}JKLMNOPQR";
    private static final String GNUCOBOL_NEGATIVE = "pqrstuvwxy";

    private Pic() {
    }

    /** MOVE to PIC X(n): left-justified, space-padded, truncated on the right. */
    public static String alpha(String value, int length) {
        String v = value == null ? "" : value;
        if (v.length() >= length) {
            return v.substring(0, length);
        }
        return v + " ".repeat(length - v.length());
    }

    /** MOVE to PIC 9(n): absolute value, high-order digits truncated, zero-padded. */
    public static String unsigned(long value, int digits) {
        long mod = pow10(digits);
        long v = Math.abs(value) % mod;
        String s = Long.toString(v);
        return "0".repeat(digits - s.length()) + s;
    }

    public static long parseUnsigned(String field) {
        if (field.isEmpty() || !field.chars().allMatch(Character::isDigit)) {
            throw new IllegalArgumentException("Non-numeric PIC 9 field: '" + field + "'");
        }
        return Long.parseLong(field);
    }

    /** Truncate to PIC S9(intDigits)V9(scale) the way a COBOL MOVE/COMPUTE without ROUNDED/SIZE ERROR does. */
    public static BigDecimal truncate(BigDecimal value, int intDigits, int scale) {
        BigDecimal v = value.setScale(scale, RoundingMode.DOWN);
        BigDecimal limit = BigDecimal.TEN.pow(intDigits);
        if (v.abs().compareTo(limit) >= 0) {
            v = v.remainder(limit);
        }
        return v.setScale(scale, RoundingMode.DOWN);
    }

    /** Encode PIC S9(intDigits)V9(scale) DISPLAY with trailing overpunch sign. */
    public static String signedZoned(BigDecimal value, int intDigits, int scale) {
        BigDecimal v = truncate(value, intDigits, scale);
        boolean negative = v.signum() < 0;
        String digits = v.abs().movePointRight(scale).toBigIntegerExact().toString();
        int width = intDigits + scale;
        digits = "0".repeat(width - digits.length()) + digits;
        int last = digits.charAt(width - 1) - '0';
        char sign = negative ? NEGATIVE_OVERPUNCH.charAt(last) : POSITIVE_OVERPUNCH.charAt(last);
        return digits.substring(0, width - 1) + sign;
    }

    /** Decode PIC S9(n)V9(scale) DISPLAY; accepts overpunch ('{', 'A'-'I', '}', 'J'-'R', 'p'-'y') or a plain digit. */
    public static BigDecimal parseSignedZoned(String field, int scale) {
        if (field.isEmpty()) {
            throw new IllegalArgumentException("Empty signed numeric field");
        }
        String head = field.substring(0, field.length() - 1);
        char last = field.charAt(field.length() - 1);
        if (!head.chars().allMatch(Character::isDigit)) {
            throw new IllegalArgumentException("Non-numeric signed field: '" + field + "'");
        }
        int digit;
        boolean negative = false;
        if (Character.isDigit(last)) {
            digit = last - '0';
        } else if (POSITIVE_OVERPUNCH.indexOf(last) >= 0) {
            digit = POSITIVE_OVERPUNCH.indexOf(last);
        } else if (NEGATIVE_OVERPUNCH.indexOf(last) >= 0) {
            digit = NEGATIVE_OVERPUNCH.indexOf(last);
            negative = true;
        } else if (GNUCOBOL_NEGATIVE.indexOf(last) >= 0) {
            digit = GNUCOBOL_NEGATIVE.indexOf(last);
            negative = true;
        } else {
            throw new IllegalArgumentException("Invalid sign character in field: '" + field + "'");
        }
        BigDecimal v = new BigDecimal(new java.math.BigInteger(head + digit), scale);
        return negative ? v.negate() : v;
    }

    /**
     * STRING ... DELIMITED BY SIZE INTO target: overwrites from position 1 and leaves the rest of the
     * receiving field unchanged (STRING never pads).
     */
    public static String stringInto(String target, int length, String... parts) {
        String joined = String.join("", parts);
        String base = alpha(target, length);
        if (joined.length() >= length) {
            return joined.substring(0, length);
        }
        return joined + base.substring(joined.length());
    }

    static long pow10(int n) {
        long r = 1;
        for (int i = 0; i < n; i++) {
            r *= 10;
        }
        return r;
    }

    /** Sequential cursor over a fixed-width record image. */
    public static final class Cursor {
        private final String image;
        private int pos;

        public Cursor(String image, int recordLength) {
            if (image.length() != recordLength) {
                throw new IllegalArgumentException(
                        "Record length " + image.length() + " != expected " + recordLength);
            }
            this.image = image;
        }

        public String take(int length) {
            String s = image.substring(pos, pos + length);
            pos += length;
            return s;
        }
    }
}
