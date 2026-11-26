package com.carddemo.programs.corpt00c;

import java.math.BigDecimal;
import java.math.RoundingMode;

/** COBOL alphanumeric / display-numeric helpers shared by the CORPT00C port. */
public final class CobolText {

    public static final char LOW_VALUE = '\u0000';

    private CobolText() {
    }

    public static String spaces(int len) {
        return " ".repeat(len);
    }

    public static String lowValues(int len) {
        return String.valueOf(LOW_VALUE).repeat(len);
    }

    /** MOVE alphanumeric: left-justify, truncate on the right or pad with spaces. A null source moves LOW-VALUES. */
    public static String move(String src, int len) {
        if (src == null) {
            return lowValues(len);
        }
        if (src.length() >= len) {
            return src.substring(0, len);
        }
        return src + spaces(len - src.length());
    }

    public static boolean isSpaces(String s) {
        return allChars(s, ' ');
    }

    public static boolean isLowValues(String s) {
        return allChars(s, LOW_VALUE);
    }

    /** {@code field = SPACES OR LOW-VALUES}. */
    public static boolean isSpacesOrLowValues(String s) {
        return isSpaces(s) || isLowValues(s);
    }

    private static boolean allChars(String s, char c) {
        for (int i = 0; i < s.length(); i++) {
            if (s.charAt(i) != c) {
                return false;
            }
        }
        return true;
    }

    /** Class test {@code IS NUMERIC} on a PIC X field: every byte is 0-9. */
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

    /** STRING ... DELIMITED BY SPACE: the characters before the first space. */
    public static String delimitedBySpace(String s) {
        int i = s.indexOf(' ');
        return i < 0 ? s : s.substring(0, i);
    }

    /**
     * STRING ... INTO target (no POINTER): overlays the concatenated parts on the target from position 1,
     * leaving any remaining target characters unchanged and dropping overflow.
     */
    public static String stringInto(String target, String... parts) {
        StringBuilder src = new StringBuilder();
        for (String p : parts) {
            src.append(p);
        }
        if (src.length() >= target.length()) {
            return src.substring(0, target.length());
        }
        return src + target.substring(src.length());
    }

    /**
     * FUNCTION NUMVAL-C with the default currency sign '$'. Accepts leading/trailing spaces, a leading
     * or trailing sign (+, -, CR, DB), a currency sign, comma grouping and a decimal point. An argument
     * that does not satisfy the NUMVAL-C syntax rules yields 0 (the result is undefined in IBM COBOL;
     * 0 matches GnuCOBOL and the observable CORPT00C behaviour of turning bad input into '00').
     */
    public static BigDecimal numvalC(String arg) {
        String s = arg;
        int n = s.length();
        int i = skipSpaces(s, 0);
        Boolean negative = null;
        boolean currency = false;
        for (int k = 0; k < 2 && i < n; k++) {
            char c = s.charAt(i);
            if ((c == '+' || c == '-') && negative == null) {
                negative = c == '-';
                i = skipSpaces(s, i + 1);
            } else if (c == '$' && !currency) {
                currency = true;
                i = skipSpaces(s, i + 1);
            }
        }
        StringBuilder digits = new StringBuilder();
        boolean point = false;
        boolean anyDigit = false;
        while (i < n) {
            char c = s.charAt(i);
            if (c >= '0' && c <= '9') {
                digits.append(c);
                anyDigit = true;
            } else if (c == ',' && !point && anyDigit) {
                // grouping separator
            } else if (c == '.' && !point) {
                point = true;
                digits.append('.');
            } else {
                break;
            }
            i++;
        }
        if (!anyDigit) {
            return BigDecimal.ZERO;
        }
        i = skipSpaces(s, i);
        if (!currency && i < n && s.charAt(i) == '$') {
            currency = true;
            i = skipSpaces(s, i + 1);
        }
        if (negative == null && i < n) {
            char c = s.charAt(i);
            String rest = s.substring(i).toUpperCase();
            if (c == '+' || c == '-') {
                negative = c == '-';
                i = skipSpaces(s, i + 1);
            } else if (rest.startsWith("CR") || rest.startsWith("DB")) {
                negative = true;
                i = skipSpaces(s, i + 2);
            }
        }
        if (i != n) {
            return BigDecimal.ZERO;
        }
        BigDecimal v = new BigDecimal(digits.toString());
        return Boolean.TRUE.equals(negative) ? v.negate() : v;
    }

    private static int skipSpaces(String s, int i) {
        while (i < s.length() && s.charAt(i) == ' ') {
            i++;
        }
        return i;
    }

    /**
     * Store a numeric value into an unsigned display field PIC 9(n) (COMPUTE without ROUNDED / SIZE ERROR):
     * sign dropped, fraction truncated, high-order digits truncated.
     */
    public static String toUnsignedDisplay(BigDecimal value, int digits) {
        BigDecimal i = value.abs().setScale(0, RoundingMode.DOWN);
        String s = i.toPlainString();
        if (s.length() > digits) {
            s = s.substring(s.length() - digits);
        }
        return "0".repeat(digits - s.length()) + s;
    }

    private static final String POS_OVERPUNCH = "{ABCDEFGHI";
    private static final String NEG_OVERPUNCH = "}JKLMNOPQR";

    /** DISPLAY of a signed numeric item: zoned digits with the sign over-punched on the last digit. */
    public static String displaySigned(long value, int digits) {
        String abs = Long.toString(Math.abs(value));
        if (abs.length() > digits) {
            abs = abs.substring(abs.length() - digits);
        }
        abs = "0".repeat(digits - abs.length()) + abs;
        int last = abs.charAt(digits - 1) - '0';
        char punch = value < 0 ? NEG_OVERPUNCH.charAt(last) : POS_OVERPUNCH.charAt(last);
        return abs.substring(0, digits - 1) + punch;
    }

    /** Decode a signed zoned-decimal PIC S9(n)V9(scale) field with trailing over-punched sign (ASCII form). */
    public static BigDecimal decodeZonedSigned(String text, int scale) {
        int lastIdx = text.length() - 1;
        char last = text.charAt(lastIdx);
        boolean negative;
        int lastDigit;
        if (last >= '0' && last <= '9') {
            negative = false;
            lastDigit = last - '0';
        } else if (POS_OVERPUNCH.indexOf(last) >= 0) {
            negative = false;
            lastDigit = POS_OVERPUNCH.indexOf(last);
        } else if (NEG_OVERPUNCH.indexOf(last) >= 0) {
            negative = true;
            lastDigit = NEG_OVERPUNCH.indexOf(last);
        } else {
            throw new NumberFormatException("Invalid zoned sign '" + last + "' in '" + text + "'");
        }
        String digits = text.substring(0, lastIdx) + lastDigit;
        if (!isNumeric(digits)) {
            throw new NumberFormatException("Invalid zoned digits in '" + text + "'");
        }
        BigDecimal v = new BigDecimal(new java.math.BigInteger(digits), scale);
        return negative ? v.negate() : v;
    }

    /** Encode a value as PIC S9(intDigits)V9(scale) zoned decimal with trailing over-punched sign. */
    public static String encodeZonedSigned(BigDecimal value, int intDigits, int scale) {
        int total = intDigits + scale;
        String digits = value.abs().setScale(scale, RoundingMode.DOWN).unscaledValue().toString();
        if (digits.length() > total) {
            digits = digits.substring(digits.length() - total);
        }
        digits = "0".repeat(total - digits.length()) + digits;
        int last = digits.charAt(total - 1) - '0';
        char punch = value.signum() < 0 ? NEG_OVERPUNCH.charAt(last) : POS_OVERPUNCH.charAt(last);
        return digits.substring(0, total - 1) + punch;
    }
}
