package com.carddemo.programs.copaua0c.support;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/** COBOL MOVE / COMPUTE / editing semantics used by COPAUA0C. */
public final class Cobol {

    private static final Pattern NUMVAL =
            Pattern.compile("^\\s*([+-])?\\s*(\\d*)(?:\\.(\\d*))?\\s*([+-]|CR|DB|cr|db)?\\s*$");

    private Cobol() {
    }

    /** Alphanumeric MOVE into PIC X(n): left-justify, pad with spaces, truncate on the right. */
    public static String alpha(String value, int length) {
        String v = value == null ? "" : value;
        if (v.length() >= length) {
            return v.substring(0, length);
        }
        return v + " ".repeat(length - v.length());
    }

    public static String spaces(int length) {
        return " ".repeat(length);
    }

    /** Alphanumeric MOVE into unsigned PIC 9(n) DISPLAY: right-justify, zero-fill, truncate on the left. */
    public static String numericDisplay(String value, int length) {
        String v = value == null ? "" : value;
        if (v.length() >= length) {
            return v.substring(v.length() - length);
        }
        return "0".repeat(length - v.length()) + v;
    }

    /**
     * Numeric store into S9(intDigits)V9(scale) without ROUNDED / ON SIZE ERROR:
     * low-order digits truncated, high-order digits truncated.
     */
    public static BigDecimal truncate(BigDecimal value, int intDigits, int scale) {
        BigDecimal scaled = value.setScale(scale, RoundingMode.DOWN);
        BigDecimal modulus = BigDecimal.TEN.pow(intDigits);
        return scaled.remainder(modulus).setScale(scale, RoundingMode.UNNECESSARY);
    }

    /** Binary PIC S9(n) COMP store under TRUNC(STD): value truncated to n decimal digits. */
    public static int truncateBinary(long value, int digits) {
        return (int) (value % BigInteger.TEN.pow(digits).longValueExact());
    }

    /** MOVE of a numeric value to PIC 9(9) DISPLAY (WS-CODE-DISPLAY): sign dropped, 9 digits. */
    public static String unsignedDisplay9(long value) {
        long abs = Math.abs(value) % 1_000_000_000L;
        return String.format(Locale.ROOT, "%09d", abs);
    }

    /** FUNCTION NUMVAL. Non-conforming arguments yield zero (IBM leaves the result undefined). */
    public static BigDecimal numval(String argument) {
        if (argument == null) {
            return BigDecimal.ZERO;
        }
        Matcher m = NUMVAL.matcher(argument);
        if (!m.matches()) {
            return BigDecimal.ZERO;
        }
        String lead = m.group(1);
        String intPart = m.group(2);
        String frac = m.group(3) == null ? "" : m.group(3);
        String trail = m.group(4);
        if (lead != null && trail != null) {
            return BigDecimal.ZERO;
        }
        if (intPart.isEmpty() && frac.isEmpty()) {
            return BigDecimal.ZERO;
        }
        BigDecimal v = new BigDecimal((intPart.isEmpty() ? "0" : intPart) + (frac.isEmpty() ? "" : "." + frac));
        boolean negative = "-".equals(lead) || (trail != null && !"+".equals(trail));
        return negative ? v.negate() : v;
    }

    /** Edit into PIC +9(10).99 (14 chars). Value must already be truncated to S9(10)V99. */
    public static String editPlus10v2(BigDecimal value) {
        BigDecimal v = truncate(value, 10, 2);
        String digits = v.abs().movePointRight(2).toBigIntegerExact().toString();
        digits = "0".repeat(12 - digits.length()) + digits;
        char sign = v.signum() < 0 ? '-' : '+';
        return sign + digits.substring(0, 10) + "." + digits.substring(10);
    }

    /** De-editing MOVE from PIC +9(10).99 to a numeric item. */
    public static BigDecimal deEditPlus10v2(String edited) {
        String e = edited.trim();
        if (e.isEmpty()) {
            return BigDecimal.ZERO.setScale(2);
        }
        try {
            return new BigDecimal(e).setScale(2, RoundingMode.DOWN);
        } catch (NumberFormatException ex) {
            return BigDecimal.ZERO.setScale(2);
        }
    }

    /** Edit into PIC -ZZZZZZZZZ9.99 (14 chars): fixed '-' / space, zero suppression on 9 positions. */
    public static String editMinusZ9v2(BigDecimal value) {
        BigDecimal v = truncate(value, 10, 2);
        String digits = v.abs().movePointRight(2).toBigIntegerExact().toString();
        digits = "0".repeat(12 - digits.length()) + digits;
        String intPart = digits.substring(0, 10);
        StringBuilder sb = new StringBuilder();
        boolean suppress = true;
        for (int i = 0; i < 10; i++) {
            char c = intPart.charAt(i);
            if (suppress && c == '0' && i < 9) {
                sb.append(' ');
            } else {
                suppress = false;
                sb.append(c);
            }
        }
        char sign = v.signum() < 0 ? '-' : ' ';
        return sign + sb.toString() + "." + digits.substring(10);
    }

    /** Parse a signed zoned-decimal DISPLAY field (trailing overpunch, ASCII convention). */
    public static BigDecimal zoned(String field, int scale) {
        String f = field.trim();
        if (f.isEmpty()) {
            return BigDecimal.ZERO.setScale(scale);
        }
        char last = f.charAt(f.length() - 1);
        String body = f.substring(0, f.length() - 1);
        boolean negative = false;
        char digit;
        if (last >= '0' && last <= '9') {
            digit = last;
        } else if (last == '{') {
            digit = '0';
        } else if (last >= 'A' && last <= 'I') {
            digit = (char) ('1' + (last - 'A'));
        } else if (last == '}') {
            digit = '0';
            negative = true;
        } else if (last >= 'J' && last <= 'R') {
            digit = (char) ('1' + (last - 'J'));
            negative = true;
        } else {
            throw new IllegalArgumentException("Invalid zoned decimal: " + field);
        }
        BigDecimal v = new BigDecimal(new BigInteger(body + digit), scale);
        return negative ? v.negate() : v;
    }

    /** Parse an unsigned PIC 9(n) DISPLAY field; spaces are treated as zero. */
    public static long unsigned(String field) {
        String f = field.trim();
        return f.isEmpty() ? 0L : Long.parseLong(f);
    }
}
