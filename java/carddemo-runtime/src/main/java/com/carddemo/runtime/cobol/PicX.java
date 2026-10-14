package com.carddemo.runtime.cobol;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

/**
 * Helpers for COBOL alphanumeric (PIC X(n)) semantics.
 *
 * <p>All CardDemo storage is modelled as single-byte characters in ISO-8859-1 so that every byte value
 * (including LOW-VALUES X'00' and HIGH-VALUES X'FF') round-trips through a {@link String} unchanged.
 */
public final class PicX {

    /** Charset used for all emulated storage: one byte per character, every byte value representable. */
    public static final Charset CHARSET = StandardCharsets.ISO_8859_1;

    public static final char SPACE = ' ';
    public static final char LOW_VALUE = '\u0000';
    public static final char HIGH_VALUE = '\u00FF';

    private PicX() {
    }

    /** MOVE alphanumeric source TO PIC X(length): left-justify, pad with spaces, truncate on the right. */
    public static String of(String value, int length) {
        String v = value == null ? "" : value;
        if (v.length() == length) {
            return v;
        }
        if (v.length() > length) {
            return v.substring(0, length);
        }
        return v + spaces(length - v.length());
    }

    /** MOVE alphanumeric source TO PIC X(length) JUSTIFIED RIGHT: right-justify, pad/truncate on the left. */
    public static String justifiedRight(String value, int length) {
        String v = value == null ? "" : value;
        if (v.length() >= length) {
            return v.substring(v.length() - length);
        }
        return spaces(length - v.length()) + v;
    }

    public static String spaces(int length) {
        return repeat(SPACE, length);
    }

    public static String lowValues(int length) {
        return repeat(LOW_VALUE, length);
    }

    public static String highValues(int length) {
        return repeat(HIGH_VALUE, length);
    }

    public static String repeat(char c, int length) {
        char[] chars = new char[length];
        Arrays.fill(chars, c);
        return new String(chars);
    }

    /** {@code field = SPACES}: true when every character is a space (an empty field is also all spaces). */
    public static boolean isSpaces(String value) {
        return allEqual(value, SPACE);
    }

    /** {@code field = LOW-VALUES}. */
    public static boolean isLowValues(String value) {
        return allEqual(value, LOW_VALUE);
    }

    /** {@code field = HIGH-VALUES}. */
    public static boolean isHighValues(String value) {
        return allEqual(value, HIGH_VALUE);
    }

    /** {@code field = SPACES OR LOW-VALUES} (each figurative constant compared against the whole field). */
    public static boolean isSpacesOrLowValues(String value) {
        return isSpaces(value) || isLowValues(value);
    }

    private static boolean allEqual(String value, char c) {
        for (int i = 0; i < value.length(); i++) {
            if (value.charAt(i) != c) {
                return false;
            }
        }
        return true;
    }

    /**
     * {@code FUNCTION UPPER-CASE}: only the 26 basic Latin lowercase letters are converted; every other
     * byte (digits, punctuation, LOW-VALUES, national characters) is left unchanged.
     */
    public static String upperCase(String value) {
        char[] chars = value.toCharArray();
        for (int i = 0; i < chars.length; i++) {
            if (chars[i] >= 'a' && chars[i] <= 'z') {
                chars[i] = (char) (chars[i] - ('a' - 'A'));
            }
        }
        return new String(chars);
    }

    /** {@code FUNCTION LOWER-CASE}: only A-Z converted. */
    public static String lowerCase(String value) {
        char[] chars = value.toCharArray();
        for (int i = 0; i < chars.length; i++) {
            if (chars[i] >= 'A' && chars[i] <= 'Z') {
                chars[i] = (char) (chars[i] + ('a' - 'A'));
            }
        }
        return new String(chars);
    }

    /**
     * Alphanumeric comparison of two fields of possibly different length: the shorter operand is padded
     * with spaces (COBOL rule). Collation is the ISO-8859-1 byte order (not EBCDIC).
     */
    public static int compare(String a, String b) {
        int len = Math.max(a.length(), b.length());
        String pa = of(a, len);
        String pb = of(b, len);
        for (int i = 0; i < len; i++) {
            int d = Character.compare(pa.charAt(i), pb.charAt(i));
            if (d != 0) {
                return d;
            }
        }
        return 0;
    }

    /** True when a and b are equal under COBOL alphanumeric comparison rules (space padding). */
    public static boolean equal(String a, String b) {
        return compare(a, b) == 0;
    }

    public static byte[] toBytes(String value) {
        return value.getBytes(CHARSET);
    }

    public static String fromBytes(byte[] bytes) {
        return new String(bytes, CHARSET);
    }

    /** Right-trim spaces only (useful when comparing line-sequential output, which drops trailing spaces). */
    public static String rtrim(String value) {
        int end = value.length();
        while (end > 0 && value.charAt(end - 1) == SPACE) {
            end--;
        }
        return value.substring(0, end);
    }
}
