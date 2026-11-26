package com.carddemo.programs.cotrtlic.support;

import java.nio.charset.Charset;
import java.util.Arrays;

/**
 * COBOL data-semantics helpers. Alphanumeric fields are fixed-length Java strings in which
 * {@code '\u0000'} represents LOW-VALUES.
 */
public final class Cob {

    public static final char LOW_VALUE = '\u0000';
    public static final char SPACE = ' ';

    private static final Charset EBCDIC = Charset.forName("IBM037");

    private Cob() {
    }

    public static String spaces(int len) {
        return fill(SPACE, len);
    }

    public static String lowValues(int len) {
        return fill(LOW_VALUE, len);
    }

    public static String fill(char c, int len) {
        char[] a = new char[len];
        Arrays.fill(a, c);
        return new String(a);
    }

    /** MOVE alphanumeric: left-justify, truncate on the right or pad with spaces. */
    public static String move(String src, int len) {
        String s = src == null ? "" : src;
        if (s.length() >= len) {
            return s.substring(0, len);
        }
        return s + spaces(len - s.length());
    }

    public static boolean isAll(String s, char c) {
        for (int i = 0; i < s.length(); i++) {
            if (s.charAt(i) != c) {
                return false;
            }
        }
        return true;
    }

    public static boolean isLowValues(String s) {
        return isAll(s, LOW_VALUE);
    }

    public static boolean isSpaces(String s) {
        return isAll(s, SPACE);
    }

    /** Alphanumeric comparison: the shorter operand is padded on the right with spaces. */
    public static boolean eq(String a, String b) {
        int len = Math.max(a.length(), b.length());
        return move(a, len).equals(move(b, len));
    }

    /** FUNCTION TRIM: removes leading and trailing spaces only (LOW-VALUES are kept). */
    public static String trim(String s) {
        int start = 0;
        int end = s.length();
        while (start < end && s.charAt(start) == SPACE) {
            start++;
        }
        while (end > start && s.charAt(end - 1) == SPACE) {
            end--;
        }
        return s.substring(start, end);
    }

    /** Class test NUMERIC on an unsigned alphanumeric item: every character is 0-9. */
    public static boolean isNumeric(String s) {
        if (s.isEmpty()) {
            return false;
        }
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c < '0' || c > '9') {
                return false;
            }
        }
        return true;
    }

    /** INSPECT ... TALLYING counter FOR ALL c. */
    public static int tally(String s, char... chars) {
        int n = 0;
        for (int i = 0; i < s.length(); i++) {
            for (char c : chars) {
                if (s.charAt(i) == c) {
                    n++;
                    break;
                }
            }
        }
        return n;
    }

    /** INSPECT ... CONVERTING from TO to (same-length translation tables). */
    public static String convert(String s, String from, String to) {
        StringBuilder sb = new StringBuilder(s.length());
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            int idx = from.indexOf(c);
            sb.append(idx >= 0 ? to.charAt(idx) : c);
        }
        return sb.toString();
    }

    /** Store into a signed numeric item of {@code digits} digits: high-order truncation, sign kept. */
    public static int truncSigned(long value, int digits) {
        return (int) truncSignedLong(value, digits);
    }

    public static long truncSignedLong(long value, int digits) {
        return value % pow10(digits);
    }

    /** Store into an unsigned numeric item of {@code digits} digits: sign lost, high-order truncation. */
    public static int truncUnsigned(long value, int digits) {
        return (int) truncUnsignedLong(value, digits);
    }

    public static long truncUnsignedLong(long value, int digits) {
        return Math.abs(value) % pow10(digits);
    }

    private static long pow10(int digits) {
        long m = 1;
        for (int i = 0; i < digits; i++) {
            m *= 10;
        }
        return m;
    }

    /** PIC 9(n) DISPLAY rendering (zero padded). */
    public static String zeroPad(long value, int digits) {
        String s = Long.toString(truncUnsignedLong(value, digits));
        return fill('0', digits - s.length()) + s;
    }

    /**
     * Numeric-edited PIC ----9 (floating minus, 5 positions). The leftmost '-' only holds the sign,
     * so at most 4 digits are kept.
     */
    public static String editFloatingMinus5(long value) {
        long abs = Math.abs(value) % 10_000;
        String digits = Long.toString(abs);
        String body = value < 0 && abs != 0 ? "-" + digits : digits;
        return fill(SPACE, 5 - body.length()) + body;
    }

    /** Compares two CHAR values in EBCDIC (code page 037) collating sequence, as DB2 for z/OS does. */
    public static int compareEbcdic(String a, String b) {
        byte[] x = a.getBytes(EBCDIC);
        byte[] y = b.getBytes(EBCDIC);
        int len = Math.max(x.length, y.length);
        for (int i = 0; i < len; i++) {
            int cx = i < x.length ? x[i] & 0xFF : 0x40;
            int cy = i < y.length ? y[i] & 0xFF : 0x40;
            if (cx != cy) {
                return Integer.compare(cx, cy);
            }
        }
        return 0;
    }

    /** Overlays {@code src} into {@code target} at 0-based {@code offset} (reference modification MOVE). */
    public static String overlay(String target, int offset, String src) {
        if (src.isEmpty()) {
            return target;
        }
        StringBuilder sb = new StringBuilder(target);
        for (int i = 0; i < src.length() && offset + i < sb.length(); i++) {
            sb.setCharAt(offset + i, src.charAt(i));
        }
        return sb.toString();
    }

    /** STRING ... DELIMITED BY SIZE INTO target: writes from position 1, leaves the rest untouched. */
    public static String stringInto(String target, String... parts) {
        return overlay(target, 0, String.join("", parts));
    }
}
