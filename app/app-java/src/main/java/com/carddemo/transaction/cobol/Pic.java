package com.carddemo.transaction.cobol;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * COBOL MOVE semantics for elementary items.
 * <ul>
 *   <li>PIC X(n): left-justified, right-truncated, space-padded to exactly n characters.</li>
 *   <li>PIC S9(i)V9(s): decimal point aligned, excess fraction digits dropped (no rounding),
 *       high-order integer digits truncated (no ON SIZE ERROR).</li>
 *   <li>PIC 9(n): as above but unsigned - the sign is lost.</li>
 * </ul>
 */
public final class Pic {

    private Pic() {
    }

    public static String x(String value, int length) {
        String v = value == null ? "" : value;
        if (v.length() >= length) {
            return v.substring(0, length);
        }
        return v + " ".repeat(length - v.length());
    }

    public static String spaces(int length) {
        return " ".repeat(length);
    }

    public static BigDecimal s9(BigDecimal value, int integerDigits, int scale) {
        if (value == null) {
            return BigDecimal.ZERO.setScale(scale);
        }
        BigDecimal scaled = value.setScale(scale, RoundingMode.DOWN);
        return scaled.remainder(BigDecimal.TEN.pow(integerDigits)).setScale(scale, RoundingMode.UNNECESSARY);
    }

    public static long n9(long value, int digits) {
        return Math.abs(value) % (long) Math.pow(10, digits);
    }

    public static int n9(int value, int digits) {
        return (int) n9((long) value, digits);
    }

    /** Numeric DISPLAY rendering of PIC 9(n): zero-filled to n digits. */
    public static String zeroFill(long value, int digits) {
        return String.format("%0" + digits + "d", n9(value, digits));
    }

    /** Equivalent of the COBOL class test {@code IS NUMERIC} on an alphanumeric item. */
    public static boolean isNumeric(String value) {
        if (value == null || value.isEmpty()) {
            return false;
        }
        for (int i = 0; i < value.length(); i++) {
            if (!Character.isDigit(value.charAt(i)) || value.charAt(i) > '9') {
                return false;
            }
        }
        return true;
    }

    /** {@code = SPACES OR LOW-VALUES}. */
    public static boolean isBlank(String value) {
        if (value == null) {
            return true;
        }
        for (int i = 0; i < value.length(); i++) {
            char c = value.charAt(i);
            if (c != ' ' && c != '\u0000') {
                return false;
            }
        }
        return true;
    }

    public static String trimRight(String value) {
        return value == null ? null : value.stripTrailing();
    }
}
