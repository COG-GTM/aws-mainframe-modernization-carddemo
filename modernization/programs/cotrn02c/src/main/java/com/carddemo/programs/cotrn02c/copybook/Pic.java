package com.carddemo.programs.cotrn02c.copybook;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

/** COBOL MOVE / figurative-constant / class-test helpers for DISPLAY data. */
public final class Pic {

    public static final char LOW_VALUE = '\u0000';
    public static final char HIGH_VALUE = '\u00FF';

    private Pic() {
    }

    public static String spaces(int n) {
        return " ".repeat(n);
    }

    public static String lowValues(int n) {
        return String.valueOf(LOW_VALUE).repeat(n);
    }

    public static String highValues(int n) {
        return String.valueOf(HIGH_VALUE).repeat(n);
    }

    public static String zeros(int n) {
        return "0".repeat(n);
    }

    /** Alphanumeric MOVE: left-justify, pad with spaces, truncate on the right. */
    public static String x(String value, int len) {
        String v = value == null ? "" : value;
        if (v.length() >= len) {
            return v.substring(0, len);
        }
        return v + spaces(len - v.length());
    }

    /** {@code field = SPACES}: every character is a space. */
    public static boolean isSpaces(String v) {
        return v.chars().allMatch(c -> c == ' ');
    }

    /** {@code field = LOW-VALUES}: every character is X'00'. */
    public static boolean isLowValues(String v) {
        return v.chars().allMatch(c -> c == LOW_VALUE);
    }

    /** {@code field = HIGH-VALUES}: every character is X'FF'. */
    public static boolean isHighValues(String v) {
        return v.chars().allMatch(c -> c == HIGH_VALUE);
    }

    /** {@code field = SPACES OR LOW-VALUES}. */
    public static boolean isSpacesOrLowValues(String v) {
        return isSpaces(v) || isLowValues(v);
    }

    /** {@code field IS NUMERIC} for an alphanumeric (PIC X) item: digits 0-9 only. */
    public static boolean isNumeric(String v) {
        return !v.isEmpty() && v.chars().allMatch(c -> c >= '0' && c <= '9');
    }

    /** Unsigned PIC 9(n) DISPLAY image of a value (high-order truncation, sign dropped). */
    public static String nine(long value, int len) {
        BigInteger mod = BigInteger.TEN.pow(len);
        BigInteger v = BigInteger.valueOf(value).abs().mod(mod);
        String s = v.toString();
        return zeros(len - s.length()) + s;
    }

    /** Unsigned PIC 9(n) DISPLAY image of a BigInteger (high-order truncation, sign dropped). */
    public static String nine(BigInteger value, int len) {
        BigInteger v = value.abs().mod(BigInteger.TEN.pow(len));
        String s = v.toString();
        return zeros(len - s.length()) + s;
    }

    /**
     * MOVE alphanumeric TO PIC 9(n): the source is treated as an unsigned integer; each byte
     * contributes its low-order nibble (as the IBM compiler does when the zone is replaced).
     */
    public static BigInteger alphaToUnsigned(String v) {
        BigInteger r = BigInteger.ZERO;
        for (char c : v.toCharArray()) {
            r = r.multiply(BigInteger.TEN).add(BigInteger.valueOf((c & 0x0F) % 10));
        }
        return r;
    }

    /** Truncate a decimal to PIC S9(intDigits)V9(scale): low-order digits dropped, high-order truncated. */
    public static BigDecimal fit(BigDecimal value, int intDigits, int scale) {
        BigDecimal v = value.setScale(scale, RoundingMode.DOWN);
        BigInteger unscaled = v.unscaledValue();
        BigInteger mod = BigInteger.TEN.pow(intDigits + scale);
        BigInteger abs = unscaled.abs().mod(mod);
        return new BigDecimal(unscaled.signum() < 0 ? abs.negate() : abs, scale);
    }

    /**
     * Zoned-decimal (DISPLAY, trailing overpunched sign) image of PIC S9(intDigits)V9(scale),
     * ASCII convention: {@code {A-I} = +0..+9, }J-R} = -0..-9}.
     */
    public static String zonedSigned(BigDecimal value, int intDigits, int scale) {
        BigDecimal v = fit(value, intDigits, scale);
        String digits = v.unscaledValue().abs().toString();
        int len = intDigits + scale;
        digits = zeros(len - digits.length()) + digits;
        int last = digits.charAt(len - 1) - '0';
        char sign = v.signum() < 0 ? "}JKLMNOPQR".charAt(last) : "{ABCDEFGHI".charAt(last);
        return digits.substring(0, len - 1) + sign;
    }

    /** Parse a zoned-decimal signed image (trailing overpunch or plain digit). */
    public static BigDecimal parseZonedSigned(String image, int scale) {
        int len = image.length();
        char last = image.charAt(len - 1);
        int digit;
        boolean negative = false;
        int pos = "{ABCDEFGHI".indexOf(last);
        if (pos >= 0) {
            digit = pos;
        } else if ((pos = "}JKLMNOPQR".indexOf(last)) >= 0) {
            digit = pos;
            negative = true;
        } else {
            digit = (last & 0x0F) % 10;
        }
        BigInteger unscaled = alphaToUnsigned(image.substring(0, len - 1))
                .multiply(BigInteger.TEN).add(BigInteger.valueOf(digit));
        return new BigDecimal(negative ? unscaled.negate() : unscaled, scale);
    }

    /**
     * MOVE numeric TO a {@code +99999999.99}-style edited picture: leading sign ('+' or '-'),
     * {@code intDigits} integer digits (high-order truncated), '.', {@code scale} decimals.
     */
    public static String signedEdited(BigDecimal value, int intDigits, int scale) {
        BigDecimal v = fit(value, intDigits, scale);
        String digits = v.unscaledValue().abs().toString();
        int len = intDigits + scale;
        digits = zeros(len - digits.length()) + digits;
        char sign = v.signum() < 0 ? '-' : '+';
        return sign + digits.substring(0, intDigits) + "." + digits.substring(intDigits);
    }

    /** FUNCTION NUMVAL: leading/trailing spaces, optional leading or trailing sign / CR / DB. */
    public static BigDecimal numval(String arg) {
        return parseNumeric(arg, false);
    }

    /** FUNCTION NUMVAL-C: as NUMVAL, additionally allowing a '$' currency sign and ',' separators. */
    public static BigDecimal numvalC(String arg) {
        return parseNumeric(arg, true);
    }

    private static BigDecimal parseNumeric(String arg, boolean currency) {
        String s = arg.replace(LOW_VALUE, ' ').trim();
        boolean negative = false;
        if (s.endsWith("CR") || s.endsWith("DB") || s.endsWith("cr") || s.endsWith("db")) {
            negative = true;
            s = s.substring(0, s.length() - 2).trim();
        }
        if (s.startsWith("-")) {
            negative = true;
            s = s.substring(1).trim();
        } else if (s.startsWith("+")) {
            s = s.substring(1).trim();
        } else if (s.endsWith("-")) {
            negative = true;
            s = s.substring(0, s.length() - 1).trim();
        } else if (s.endsWith("+")) {
            s = s.substring(0, s.length() - 1).trim();
        }
        if (currency) {
            s = s.replace("$", "").replace(",", "").trim();
        }
        if (s.isEmpty() || s.equals(".")) {
            return BigDecimal.ZERO;
        }
        BigDecimal v = new BigDecimal(s);
        return negative ? v.negate() : v;
    }
}
