package com.carddemo.programs.copaus2c.copybook;

import java.math.BigDecimal;
import java.math.RoundingMode;

/** COBOL MOVE semantics for the PIC clauses used by COPAUS2C. */
public final class Pic {

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

    /** Reference modification {@code field(start:length)} with 1-based start. */
    public static String refMod(String field, int start, int length) {
        return field.substring(start - 1, start - 1 + length);
    }

    /** MOVE / COMPUTE into unsigned PIC 9(n): absolute value, high-order truncation. */
    public static long unsigned(long value, int digits) {
        return Math.abs(value) % pow10(digits);
    }

    /** MOVE into signed PIC S9(n) (DISPLAY, COMP or COMP-3): sign kept, high-order truncation. */
    public static long signed(long value, int digits) {
        return value % pow10(digits);
    }

    /** MOVE into PIC S9(p-s)V9(s) COMP-3: scale truncated (no rounding), high-order truncation. */
    public static BigDecimal decimal(BigDecimal value, int integerDigits, int scale) {
        BigDecimal v = value.setScale(scale, RoundingMode.DOWN);
        BigDecimal limit = BigDecimal.TEN.pow(integerDigits);
        return v.remainder(limit).setScale(scale, RoundingMode.DOWN);
    }

    /** DISPLAY image of PIC 9(n). */
    public static String display9(long value, int digits) {
        return String.format(java.util.Locale.ROOT, "%0" + digits + "d", unsigned(value, digits));
    }

    /** MOVE numeric to numeric-edited PIC +9(n): leading '+' (zero or positive) or '-', then n digits. */
    public static String plusEdited(long value, int digits) {
        return (value < 0 ? "-" : "+") + display9(value, digits);
    }

    /**
     * MOVE alphanumeric to numeric-edited PIC +9(n). The sending item is treated as an
     * unsigned integer whose digit value is the low-order nibble of each EBCDIC byte
     * (so '0'-'9' map to themselves and letters to their EBCDIC digit nibble).
     */
    public static String plusEditedFromAlphanumeric(String value, int digits) {
        StringBuilder sb = new StringBuilder();
        byte[] ebcdic = value.getBytes(Ebcdic.CHARSET);
        for (byte b : ebcdic) {
            int nibble = b & 0x0F;
            sb.append((char) ('0' + (nibble > 9 ? nibble % 10 : nibble)));
        }
        String digitsOnly = sb.toString();
        if (digitsOnly.length() > digits) {
            digitsOnly = digitsOnly.substring(digitsOnly.length() - digits);
        }
        return "+" + "0".repeat(digits - digitsOnly.length()) + digitsOnly;
    }

    /**
     * {@code STRING ... DELIMITED BY SIZE INTO target}: writes the concatenation from position 1
     * and leaves any remaining characters of the target unchanged (no space padding).
     */
    public static String stringInto(String target, String... parts) {
        StringBuilder sb = new StringBuilder();
        for (String p : parts) {
            sb.append(p);
        }
        String written = sb.length() > target.length() ? sb.substring(0, target.length()) : sb.toString();
        return written + target.substring(written.length());
    }

    private static long pow10(int digits) {
        long p = 1;
        for (int i = 0; i < digits; i++) {
            p *= 10;
        }
        return p;
    }
}
