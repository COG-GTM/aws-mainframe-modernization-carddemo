package com.carddemo.programs.coactupc.support;

import java.math.BigDecimal;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Intrinsic functions NUMVAL, NUMVAL-C, TEST-NUMVAL and TEST-NUMVAL-C (Enterprise COBOL rules):
 * optional leading or trailing sign (+, -, CR, DB - not both), optional currency sign '$' for the
 * -C variants, embedded spaces around sign/currency, a decimal point '.', and for NUMVAL-C comma
 * group separators in the integer part. TEST-* return 0 when the argument is valid, otherwise the
 * 1-based position of the first invalid character (or length + 1 when the string ends too early).
 */
public final class NumericFunctions {

    private static final String NUM = "(\\d+(?:\\.\\d*)?|\\.\\d+)";
    private static final String NUM_C = "(\\d[\\d,]*(?:\\.\\d*)?|\\.\\d+)";
    private static final String TRAILING_SIGN = "(\\+|-|CR|DB|cr|db|Cr|cR|Db|dB)";

    private static final Pattern NUMVAL_LEADING = Pattern.compile("^ *(?:([+-]) *)?" + NUM + " *$");
    private static final Pattern NUMVAL_TRAILING = Pattern.compile("^ *" + NUM + " *" + TRAILING_SIGN + " *$");
    private static final Pattern NUMVALC_LEADING = Pattern.compile("^ *(?:([+-]) *)?(?:\\$ *)?" + NUM_C + " *$");
    private static final Pattern NUMVALC_TRAILING =
            Pattern.compile("^ *(?:\\$ *)?" + NUM_C + " *" + TRAILING_SIGN + " *$");

    private static final int MAX_DIGITS = 31;

    private NumericFunctions() {
    }

    public static int testNumval(String argument) {
        return test(argument, NUMVAL_LEADING, NUMVAL_TRAILING);
    }

    public static int testNumvalC(String argument) {
        return test(argument, NUMVALC_LEADING, NUMVALC_TRAILING);
    }

    public static BigDecimal numval(String argument) {
        return value(argument, NUMVAL_LEADING, NUMVAL_TRAILING);
    }

    public static BigDecimal numvalC(String argument) {
        return value(argument, NUMVALC_LEADING, NUMVALC_TRAILING);
    }

    private static int test(String argument, Pattern leading, Pattern trailing) {
        Parsed parsed = parse(argument, leading, trailing);
        if (parsed != null) {
            return 0;
        }
        return firstInvalidPosition(argument);
    }

    private static BigDecimal value(String argument, Pattern leading, Pattern trailing) {
        Parsed parsed = parse(argument, leading, trailing);
        if (parsed == null) {
            throw new NumberFormatException("Invalid NUMVAL argument: '" + argument + "'");
        }
        return parsed.value;
    }

    private record Parsed(BigDecimal value) {
    }

    private static Parsed parse(String argument, Pattern leading, Pattern trailing) {
        Matcher m = leading.matcher(argument);
        String sign;
        String number;
        if (m.matches()) {
            sign = m.group(1);
            number = m.group(2);
        } else {
            m = trailing.matcher(argument);
            if (!m.matches()) {
                return null;
            }
            number = m.group(1);
            sign = m.group(2);
        }
        String digitsOnly = number.replace(",", "");
        if (digitsOnly.replace(".", "").length() > MAX_DIGITS) {
            return null;
        }
        if (digitsOnly.endsWith(".")) {
            digitsOnly = digitsOnly + "0";
        }
        if (digitsOnly.startsWith(".")) {
            digitsOnly = "0" + digitsOnly;
        }
        BigDecimal v = new BigDecimal(digitsOnly);
        if (sign != null && (sign.equals("-") || sign.equalsIgnoreCase("CR") || sign.equalsIgnoreCase("DB"))) {
            v = v.negate();
        }
        return new Parsed(v);
    }

    private static int firstInvalidPosition(String argument) {
        for (int i = 0; i < argument.length(); i++) {
            char c = argument.charAt(i);
            if (!(Character.isDigit(c) || c == ' ' || c == '.' || c == ',' || c == '+' || c == '-' || c == '$'
                    || "CRDBcrdb".indexOf(c) >= 0)) {
                return i + 1;
            }
        }
        return argument.length() + 1;
    }
}
