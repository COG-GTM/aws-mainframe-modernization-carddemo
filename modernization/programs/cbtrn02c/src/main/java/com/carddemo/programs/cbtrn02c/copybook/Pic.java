package com.carddemo.programs.cbtrn02c.copybook;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * COBOL USAGE DISPLAY picture helpers: PIC X(n), PIC 9(n) and PIC S9(i)V9(s) zoned decimal
 * with the sign overpunched on the last digit ('{' A-I positive, '}' J-R negative), which is the
 * encoding used by the CardDemo sample data in app/data/ASCII.
 */
public final class Pic {

    private static final String POSITIVE_OVERPUNCH = "{ABCDEFGHI";
    private static final String NEGATIVE_OVERPUNCH = "}JKLMNOPQR";
    private static final String ASCII_NEGATIVE = "pqrstuvwxy";

    private Pic() {
    }

    /** MOVE to PIC X(len): left-justified, space padded, truncated on the right. */
    public static String alnum(String value, int len) {
        String v = value == null ? "" : value;
        if (v.length() >= len) {
            return v.substring(0, len);
        }
        return v + " ".repeat(len - v.length());
    }

    /** MOVE to PIC 9(digits): absolute value, high-order digits truncated, zero filled. */
    public static String unsigned(long value, int digits) {
        String s = Long.toString(Math.abs(value));
        if (s.length() > digits) {
            return s.substring(s.length() - digits);
        }
        return "0".repeat(digits - s.length()) + s;
    }

    public static long parseUnsigned(String field) {
        long v = 0;
        for (int i = 0; i < field.length(); i++) {
            char c = field.charAt(i);
            if (c < '0' || c > '9') {
                throw new NumberFormatException("Non-numeric PIC 9 field: '" + field + "'");
            }
            v = v * 10 + (c - '0');
        }
        return v;
    }

    /**
     * Truncates a value to fit PIC S9(intDigits)V9(scale) the way a MOVE / ADD / COMPUTE without
     * ROUNDED or ON SIZE ERROR does: excess decimals and high-order integer digits are dropped.
     */
    public static BigDecimal truncate(BigDecimal value, int intDigits, int scale) {
        BigDecimal scaled = value.setScale(scale, RoundingMode.DOWN);
        BigDecimal modulus = BigDecimal.TEN.pow(intDigits);
        return scaled.remainder(modulus).setScale(scale);
    }

    /** Encodes PIC S9(intDigits)V9(scale) DISPLAY with trailing overpunched sign. */
    public static String signed(BigDecimal value, int intDigits, int scale) {
        BigDecimal v = truncate(value, intDigits, scale);
        String digits = unsigned(v.unscaledValue().abs().longValueExact(), intDigits + scale);
        int last = digits.charAt(digits.length() - 1) - '0';
        char sign = v.signum() < 0 ? NEGATIVE_OVERPUNCH.charAt(last) : POSITIVE_OVERPUNCH.charAt(last);
        return digits.substring(0, digits.length() - 1) + sign;
    }

    /** Decodes PIC S9(..)V9(scale) DISPLAY; accepts EBCDIC-style overpunch, ASCII 'p'-'y' or plain digits. */
    public static BigDecimal parseSigned(String field, int scale) {
        char lastChar = field.charAt(field.length() - 1);
        int lastDigit;
        boolean negative = false;
        if (lastChar >= '0' && lastChar <= '9') {
            lastDigit = lastChar - '0';
        } else if (POSITIVE_OVERPUNCH.indexOf(lastChar) >= 0) {
            lastDigit = POSITIVE_OVERPUNCH.indexOf(lastChar);
        } else if (NEGATIVE_OVERPUNCH.indexOf(lastChar) >= 0) {
            lastDigit = NEGATIVE_OVERPUNCH.indexOf(lastChar);
            negative = true;
        } else if (ASCII_NEGATIVE.indexOf(lastChar) >= 0) {
            lastDigit = ASCII_NEGATIVE.indexOf(lastChar);
            negative = true;
        } else {
            throw new NumberFormatException("Invalid sign position in PIC S9 field: '" + field + "'");
        }
        long unscaled = parseUnsigned(field.substring(0, field.length() - 1)) * 10 + lastDigit;
        BigDecimal v = BigDecimal.valueOf(unscaled, scale);
        return negative ? v.negate() : v;
    }

    /** Sequential parser over a fixed-width record, field by field in copybook order. */
    public static final class Reader {
        private final String raw;
        private int pos;

        public Reader(String raw, int recordLength) {
            this.raw = alnum(raw, recordLength);
        }

        public String x(int len) {
            String s = raw.substring(pos, pos + len);
            pos += len;
            return s;
        }

        public long n(int len) {
            return parseUnsigned(x(len));
        }

        public BigDecimal s(int intDigits, int scale) {
            return parseSigned(x(intDigits + scale), scale);
        }
    }

    /** Sequential formatter producing a fixed-width record in copybook order. */
    public static final class Writer {
        private final StringBuilder sb = new StringBuilder();

        public Writer x(String value, int len) {
            sb.append(alnum(value, len));
            return this;
        }

        public Writer n(long value, int len) {
            sb.append(unsigned(value, len));
            return this;
        }

        public Writer s(BigDecimal value, int intDigits, int scale) {
            sb.append(signed(value, intDigits, scale));
            return this;
        }

        public String build(int recordLength) {
            if (sb.length() != recordLength) {
                throw new IllegalStateException("Record length " + sb.length() + " != " + recordLength);
            }
            return sb.toString();
        }
    }
}
