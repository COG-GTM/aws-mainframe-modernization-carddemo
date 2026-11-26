package com.carddemo.programs.cbstm03a.support;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

/**
 * COBOL PICTURE semantics used by CBSTM03A: alphanumeric moves, zoned-decimal
 * (DISPLAY, trailing EBCDIC overpunch sign) decode/encode, numeric storage
 * truncation and the two numeric-edited pictures of the statement lines.
 */
public final class Pic {

    private static final String POSITIVE_OVERPUNCH = "{ABCDEFGHI";
    private static final String NEGATIVE_OVERPUNCH = "}JKLMNOPQR";

    private Pic() {
    }

    /** MOVE to PIC X(n): left-justify, pad with spaces, truncate on the right. */
    public static String x(String value, int length) {
        String v = value == null ? "" : value;
        if (v.length() >= length) {
            return v.substring(0, length);
        }
        return v + " ".repeat(length - v.length());
    }

    /** Fixed-width slice of a record (1-based offset, as in the copybook). */
    public static String field(String record, int offset1, int length) {
        return x(record.length() >= offset1 - 1 ? record.substring(Math.min(record.length(), offset1 - 1)) : "", length);
    }

    /** PIC 9(n) DISPLAY decode; spaces read as zero. */
    public static long unsigned(String field) {
        long v = 0;
        for (int i = 0; i < field.length(); i++) {
            char c = field.charAt(i);
            v = v * 10 + (c >= '0' && c <= '9' ? c - '0' : 0);
        }
        return v;
    }

    /** PIC 9(n) DISPLAY encode (high-order truncation, no sign). */
    public static String unsigned(long value, int digits) {
        String s = Long.toString(Math.abs(value));
        if (s.length() > digits) {
            return s.substring(s.length() - digits);
        }
        return "0".repeat(digits - s.length()) + s;
    }

    /** PIC S9(i)V9(d) DISPLAY decode with trailing overpunch sign. */
    public static BigDecimal zoned(String field, int intDigits, int decDigits) {
        int width = intDigits + decDigits;
        String f = x(field, width);
        StringBuilder digits = new StringBuilder(width);
        boolean negative = false;
        for (int i = 0; i < width; i++) {
            char c = f.charAt(i);
            boolean last = i == width - 1;
            if (c >= '0' && c <= '9') {
                digits.append(c);
            } else if (last && POSITIVE_OVERPUNCH.indexOf(c) >= 0) {
                digits.append((char) ('0' + POSITIVE_OVERPUNCH.indexOf(c)));
            } else if (last && NEGATIVE_OVERPUNCH.indexOf(c) >= 0) {
                digits.append((char) ('0' + NEGATIVE_OVERPUNCH.indexOf(c)));
                negative = true;
            } else if (c == ' ') {
                digits.append('0');
            } else {
                throw new IllegalArgumentException("invalid zoned decimal byte at position " + (i + 1) + " of " + width);
            }
        }
        BigInteger unscaled = new BigInteger(digits.toString());
        return new BigDecimal(negative ? unscaled.negate() : unscaled, decDigits);
    }

    /** PIC S9(i)V9(d) DISPLAY encode with trailing overpunch sign. */
    public static String zoned(BigDecimal value, int intDigits, int decDigits) {
        BigDecimal stored = store(value, intDigits, decDigits);
        int width = intDigits + decDigits;
        String digits = stored.abs().unscaledValue().toString();
        digits = "0".repeat(width - digits.length()) + digits;
        String table = stored.signum() < 0 ? NEGATIVE_OVERPUNCH : POSITIVE_OVERPUNCH;
        return digits.substring(0, width - 1) + table.charAt(digits.charAt(width - 1) - '0');
    }

    /**
     * Value as stored in a signed numeric receiver PIC S9(i)V9(d) without ROUNDED and
     * without ON SIZE ERROR: excess decimals truncated, excess high-order digits dropped.
     */
    public static BigDecimal store(BigDecimal value, int intDigits, int decDigits) {
        BigDecimal scaled = value.setScale(decDigits, RoundingMode.DOWN);
        BigInteger modulus = BigInteger.TEN.pow(intDigits + decDigits);
        BigInteger unscaled = scaled.unscaledValue().abs().mod(modulus);
        if (scaled.signum() < 0) {
            unscaled = unscaled.negate();
        }
        return new BigDecimal(unscaled, decDigits);
    }

    /** PIC 9(n).99- : zero-filled integer digits, trailing '-' when negative else space. */
    public static String editNines(BigDecimal value, int intDigits) {
        BigDecimal stored = store(value, intDigits, 2);
        String digits = unsignedDigits(stored, intDigits);
        return digits.substring(0, intDigits) + "." + digits.substring(intDigits) + sign(stored);
    }

    /** PIC Z(n).99- : leading integer zeros suppressed to spaces, trailing sign. */
    public static String editZeroSuppressed(BigDecimal value, int intDigits) {
        BigDecimal stored = store(value, intDigits, 2);
        String digits = unsignedDigits(stored, intDigits);
        StringBuilder integer = new StringBuilder(digits.substring(0, intDigits));
        for (int i = 0; i < intDigits && integer.charAt(i) == '0'; i++) {
            integer.setCharAt(i, ' ');
        }
        return integer + "." + digits.substring(intDigits) + sign(stored);
    }

    private static String unsignedDigits(BigDecimal stored, int intDigits) {
        String digits = stored.abs().unscaledValue().toString();
        return "0".repeat(intDigits + 2 - digits.length()) + digits;
    }

    private static String sign(BigDecimal stored) {
        return stored.signum() < 0 ? "-" : " ";
    }
}
