package com.carddemo.common.batch;

import java.util.Collection;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A step's {@code COND=(code,operator)} test. JCL bypasses the step when {@code code operator RC}
 * is true for the return code of any earlier step that ran: {@code COND=(0,NE)} runs the step only
 * if every earlier step ended with RC 0, {@code COND=(4,LT)} only if none ended above 4.
 */
public record JclCond(int code, Operator operator) {

    private static final Pattern SYNTAX = Pattern.compile("\\(?\\s*(\\d{1,4})\\s*,\\s*([A-Za-z]{2})\\s*\\)?");

    public enum Operator {
        GT, GE, EQ, NE, LT, LE;

        boolean test(int left, int right) {
            return switch (this) {
                case GT -> left > right;
                case GE -> left >= right;
                case EQ -> left == right;
                case NE -> left != right;
                case LT -> left < right;
                case LE -> left <= right;
            };
        }
    }

    /** {@code COND=(0,NE)}: run only when all earlier steps returned 0. */
    public static final JclCond ALL_ZERO = new JclCond(0, Operator.NE);

    public JclCond {
        if (code < 0 || code > 4095) {
            throw new IllegalArgumentException("COND code must be 0-4095: " + code);
        }
    }

    /** Parses {@code (0,NE)} or {@code 0,NE}. */
    public static JclCond parse(String text) {
        Matcher m = SYNTAX.matcher(text.trim());
        if (!m.matches()) {
            throw new IllegalArgumentException("Not a COND=(code,op) test: " + text);
        }
        return new JclCond(Integer.parseInt(m.group(1)), Operator.valueOf(m.group(2).toUpperCase(Locale.ROOT)));
    }

    /** True when the step is bypassed given the return codes of the earlier steps. */
    public boolean bypass(Collection<Integer> priorReturnCodes) {
        return priorReturnCodes.stream().anyMatch(rc -> operator.test(code, rc));
    }

    @Override
    public String toString() {
        return "COND=(" + code + "," + operator + ")";
    }
}
