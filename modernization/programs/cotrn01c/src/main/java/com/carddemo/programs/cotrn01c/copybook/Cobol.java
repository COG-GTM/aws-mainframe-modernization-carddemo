package com.carddemo.programs.cotrn01c.copybook;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

/** Helpers reproducing COBOL MOVE / figurative-constant semantics on fixed-width display fields. */
public final class Cobol {

    public static final char LOW_VALUE = '\u0000';

    private Cobol() {
    }

    public static String spaces(int len) {
        return " ".repeat(len);
    }

    public static String lowValues(int len) {
        return String.valueOf(LOW_VALUE).repeat(len);
    }

    /** Alphanumeric MOVE: left-justify, pad with spaces, truncate on the right. */
    public static String moveAlnum(String src, int len) {
        String s = src == null ? "" : src;
        if (s.length() >= len) {
            return s.substring(0, len);
        }
        return s + spaces(len - s.length());
    }

    /** Unsigned numeric display MOVE (PIC 9(n)): keep low-order digits, zero-fill on the left. */
    public static String moveUnsignedNumeric(long value, int digits) {
        BigInteger mod = BigInteger.TEN.pow(digits);
        BigInteger v = BigInteger.valueOf(value).abs().mod(mod);
        String s = v.toString();
        return "0".repeat(digits - s.length()) + s;
    }

    /** {@code X = SPACES} : every character is a space. */
    public static boolean isSpaces(String s) {
        return s.chars().allMatch(c -> c == ' ');
    }

    /** {@code X = LOW-VALUES} : every character is X'00'. */
    public static boolean isLowValues(String s) {
        return s.chars().allMatch(c -> c == LOW_VALUE);
    }

    /** {@code X = SPACES OR LOW-VALUES} (an all-space or an all-low-value field, not a mix). */
    public static boolean isSpacesOrLowValues(String s) {
        return isSpaces(s) || isLowValues(s);
    }

    /** Parses PIC 9(n) display; non-digit characters (e.g. spaces) count as 0 like a numeric MOVE would not guarantee. */
    public static long parseUnsigned(String s) {
        long v = 0;
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            v = v * 10 + (Character.isDigit(c) ? c - '0' : 0);
        }
        return v;
    }

    /**
     * Parses a zoned-decimal PIC S9(i)V9(s) with a trailing overpunch sign
     * (IBM convention: '{'/A-I = +0..+9, '}'/J-R = -0..-9; a plain digit is unsigned/positive).
     */
    public static BigDecimal parseZonedSigned(String s, int scale) {
        char last = s.charAt(s.length() - 1);
        int digit;
        boolean negative;
        if (last == '{') {
            digit = 0;
            negative = false;
        } else if (last >= 'A' && last <= 'I') {
            digit = last - 'A' + 1;
            negative = false;
        } else if (last == '}') {
            digit = 0;
            negative = true;
        } else if (last >= 'J' && last <= 'R') {
            digit = last - 'J' + 1;
            negative = true;
        } else if (Character.isDigit(last)) {
            digit = last - '0';
            negative = false;
        } else {
            throw new IllegalArgumentException("Invalid zoned sign character: '" + last + "'");
        }
        String digits = s.substring(0, s.length() - 1) + digit;
        BigDecimal v = new BigDecimal(new BigInteger(digits), scale);
        return negative ? v.negate() : v;
    }

    /** Formats PIC S9(i)V9(s) as zoned decimal with trailing overpunch sign (high-order truncation). */
    public static String formatZonedSigned(BigDecimal value, int intDigits, int scale) {
        int total = intDigits + scale;
        BigInteger unscaled = value.setScale(scale, RoundingMode.DOWN).unscaledValue();
        boolean negative = unscaled.signum() < 0;
        String digits = unscaled.abs().mod(BigInteger.TEN.pow(total)).toString();
        digits = "0".repeat(total - digits.length()) + digits;
        int lastDigit = digits.charAt(total - 1) - '0';
        char sign = negative
                ? (lastDigit == 0 ? '}' : (char) ('J' + lastDigit - 1))
                : (lastDigit == 0 ? '{' : (char) ('A' + lastDigit - 1));
        return digits.substring(0, total - 1) + sign;
    }

    /**
     * MOVE of a numeric value to the edited picture {@code +99999999.99}: fixed '+' insertion sign
     * ('+' when zero or positive, '-' when negative), 8 integer digits (high-order digits truncated),
     * 2 decimal digits (extra low-order digits truncated, never rounded).
     */
    public static String editPlus8Dot2(BigDecimal value) {
        BigInteger unscaled = value.setScale(2, RoundingMode.DOWN).unscaledValue();
        char sign = unscaled.signum() < 0 ? '-' : '+';
        BigInteger v = unscaled.abs().mod(BigInteger.TEN.pow(10));
        String digits = v.toString();
        digits = "0".repeat(10 - digits.length()) + digits;
        return sign + digits.substring(0, 8) + "." + digits.substring(8);
    }

    /** DISPLAY of a PIC S9(09) COMP item (GnuCOBOL rendering: sign + 10 digits). */
    public static String displayBinaryS9(int value) {
        String digits = Long.toString(Math.abs((long) value));
        return (value < 0 ? "-" : "+") + "0".repeat(Math.max(0, 10 - digits.length())) + digits;
    }
}
