package com.carddemo.runtime.cobol;

import java.math.BigDecimal;

/**
 * {@code FUNCTION NUMVAL}, {@code NUMVAL-C}, {@code TEST-NUMVAL}, {@code TEST-NUMVAL-C} (IBM Enterprise COBOL
 * rules, default currency sign {@code $}, decimal point '.'):
 * <pre>
 *   NUMVAL   : [sp][+|-][sp] digits[.[digits]] | .digits [sp][+|-|CR|DB][sp]
 *   NUMVAL-C : as NUMVAL, plus an optional '$' after the leading sign (or before the digits) and ','
 *              separators in the integer part
 * </pre>
 * Only one sign is allowed; CR/DB are accepted in either case. LOW-VALUES and other characters are invalid.
 */
public final class CobolNumval {

    private CobolNumval() {
    }

    /** {@code TEST-NUMVAL}: 0 when valid, else the 1-based position of the first invalid character (length+1 if incomplete). */
    public static int testNumval(String arg) {
        return parse(arg, false).errorPosition;
    }

    /** {@code TEST-NUMVAL-C}. */
    public static int testNumvalC(String arg) {
        return parse(arg, true).errorPosition;
    }

    /** {@code NUMVAL}; callers must check {@link #testNumval} first (invalid input throws). */
    public static BigDecimal numval(String arg) {
        return value(parse(arg, false), arg);
    }

    /** {@code NUMVAL-C}; callers must check {@link #testNumvalC} first (invalid input throws). */
    public static BigDecimal numvalC(String arg) {
        return value(parse(arg, true), arg);
    }

    private static BigDecimal value(Result r, String arg) {
        if (r.errorPosition != 0) {
            throw new NumberFormatException("Invalid NUMVAL argument '" + arg + "'");
        }
        return r.value;
    }

    private record Result(int errorPosition, BigDecimal value) {
    }

    private static Result parse(String s, boolean currency) {
        int n = s.length();
        int i = 0;
        boolean negative = false;
        boolean signSeen = false;
        boolean currencySeen = false;
        while (i < n && s.charAt(i) == ' ') {
            i++;
        }
        if (i < n && (s.charAt(i) == '+' || s.charAt(i) == '-')) {
            negative = s.charAt(i) == '-';
            signSeen = true;
            i++;
            while (i < n && s.charAt(i) == ' ') {
                i++;
            }
        }
        if (currency && i < n && s.charAt(i) == '$') {
            currencySeen = true;
            i++;
            while (i < n && s.charAt(i) == ' ') {
                i++;
            }
        }
        StringBuilder digits = new StringBuilder();
        int scale = 0;
        boolean anyDigit = false;
        boolean point = false;
        while (i < n) {
            char c = s.charAt(i);
            if (c >= '0' && c <= '9') {
                digits.append(c);
                anyDigit = true;
                if (point) {
                    scale++;
                }
            } else if (c == '.' && !point) {
                point = true;
            } else if (c == ',' && currency && !point && anyDigit) {
                // thousands separator
            } else {
                break;
            }
            i++;
        }
        if (!anyDigit) {
            return new Result(i < n ? i + 1 : n + 1, null);
        }
        while (i < n && s.charAt(i) == ' ') {
            i++;
        }
        if (i < n && !signSeen) {
            char c = s.charAt(i);
            if (c == '+' || c == '-') {
                negative = c == '-';
                signSeen = true;
                i++;
            } else if (i + 1 < n && (s.substring(i, i + 2).equalsIgnoreCase("CR") || s.substring(i, i + 2).equalsIgnoreCase("DB"))) {
                negative = true;
                signSeen = true;
                i += 2;
            }
            while (i < n && s.charAt(i) == ' ') {
                i++;
            }
        }
        if (i < n) {
            return new Result(i + 1, null);
        }
        BigDecimal v = new BigDecimal(new java.math.BigInteger(digits.toString()), scale);
        return new Result(0, negative ? v.negate() : v);
    }
}
