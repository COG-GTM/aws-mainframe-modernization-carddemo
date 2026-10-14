package com.carddemo.runtime.cobol;

import java.math.BigDecimal;

/**
 * {@code FUNCTION NUMVAL}, {@code NUMVAL-C}, {@code TEST-NUMVAL}, {@code TEST-NUMVAL-C} and the alphanumeric
 * {@code IS NUMERIC} class test.
 *
 * <p>Accepted format (IBM Enterprise COBOL): optional leading spaces, an optional sign ({@code +}/{@code -}) either
 * leading or trailing ({@code +}, {@code -}, {@code CR}, {@code DB}, case-insensitive), digits with an optional
 * decimal point, trailing spaces; spaces may separate the sign from the number. NUMVAL-C additionally allows the
 * currency sign {@code $} (before the digits, either side of a leading sign) and comma separators in the integer
 * part. At least one digit is required; LOW-VALUES and other characters are invalid.
 */
public final class CobolNumval {

    private CobolNumval() {
    }

    /** {@code FUNCTION TEST-NUMVAL}: 0 when valid, else the 1-based position of the first invalid character. */
    public static int testNumval(String text) {
        return new Parser(text, false).test();
    }

    /** {@code FUNCTION TEST-NUMVAL-C} (currency sign {@code $}, comma separators). */
    public static int testNumvalC(String text) {
        return new Parser(text, true).test();
    }

    /** {@code FUNCTION NUMVAL}; throws {@link IllegalArgumentException} for invalid input. */
    public static BigDecimal numval(String text) {
        return new Parser(text, false).value();
    }

    /** {@code FUNCTION NUMVAL-C}; throws {@link IllegalArgumentException} for invalid input. */
    public static BigDecimal numvalC(String text) {
        return new Parser(text, true).value();
    }

    /** {@code alphanumeric-item IS NUMERIC}: every character is a digit 0-9 (an empty item is not numeric). */
    public static boolean isNumeric(String text) {
        if (text.isEmpty()) {
            return false;
        }
        for (int i = 0; i < text.length(); i++) {
            char c = text.charAt(i);
            if (c < '0' || c > '9') {
                return false;
            }
        }
        return true;
    }

    private static final class Parser {
        private final String s;
        private final boolean currency;
        private int pos;
        private int error;
        private boolean negative;
        private final StringBuilder digits = new StringBuilder();
        private int scale;

        Parser(String s, boolean currency) {
            this.s = s;
            this.currency = currency;
            parse();
        }

        int test() {
            return error;
        }

        BigDecimal value() {
            if (error != 0) {
                throw new IllegalArgumentException("Invalid NUMVAL argument at position " + error + ": '" + s + "'");
            }
            BigDecimal v = new BigDecimal(new java.math.BigInteger(digits.toString()), scale);
            return negative ? v.negate() : v;
        }

        private boolean at(char c) {
            return pos < s.length() && s.charAt(pos) == c;
        }

        private void spaces() {
            while (at(' ')) {
                pos++;
            }
        }

        private boolean leadingSign() {
            if (at('+') || at('-')) {
                negative = at('-');
                pos++;
                spaces();
                return true;
            }
            return false;
        }

        private void parse() {
            spaces();
            boolean signed = leadingSign();
            if (currency && at('$')) {
                pos++;
                spaces();
                if (!signed) {
                    signed = leadingSign();
                }
            }
            boolean anyDigit = false;
            boolean lastWasDigit = false;
            while (pos < s.length()) {
                char c = s.charAt(pos);
                if (c >= '0' && c <= '9') {
                    digits.append(c);
                    anyDigit = true;
                    lastWasDigit = true;
                    pos++;
                } else if (currency && c == ',' && lastWasDigit && pos + 1 < s.length()
                        && s.charAt(pos + 1) >= '0' && s.charAt(pos + 1) <= '9') {
                    lastWasDigit = false;
                    pos++;
                } else {
                    break;
                }
            }
            if (at('.')) {
                pos++;
                while (pos < s.length() && s.charAt(pos) >= '0' && s.charAt(pos) <= '9') {
                    digits.append(s.charAt(pos));
                    scale++;
                    anyDigit = true;
                    pos++;
                }
            }
            if (!anyDigit) {
                error = pos + 1;
                return;
            }
            spaces();
            if (!signed && pos < s.length()) {
                String rest = s.substring(pos).toUpperCase(java.util.Locale.ROOT);
                if (rest.startsWith("+") || rest.startsWith("-")) {
                    negative = rest.startsWith("-");
                    pos++;
                } else if (rest.startsWith("CR") || rest.startsWith("DB")) {
                    negative = true;
                    pos += 2;
                }
                spaces();
            }
            if (pos < s.length()) {
                error = pos + 1;
            }
        }
    }
}
