package com.carddemo.common.codec;

import java.math.BigDecimal;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * {@code FUNCTION TEST-NUMVAL-C} / {@code FUNCTION NUMVAL-C} with the default currency sign {@code $}, decimal point
 * {@code .} and grouping comma: optional leading or trailing sign ({@code +}, {@code -}, {@code CR}, {@code DB}), an
 * optional currency sign before or after the leading sign, spaces between the parts.
 */
public final class NumvalC {

    private static final Pattern FORMAT = Pattern.compile(
            " *(?:(?<lead1>[+-]) *(?:\\$ *)?|\\$ *(?:(?<lead2>[+-]) *)?)?"
                    + "(?<int>[0-9][0-9,]*)?(?:\\.(?<frac>[0-9]*))?"
                    + " *(?:(?<trail>[+-]|CR|DB|cr|db|Cr|cR|Db|dB) *)?");

    private NumvalC() {
    }

    /** {@code TEST-NUMVAL-C(text) = 0}. */
    public static boolean isValid(String text) {
        return parse(text).isPresent();
    }

    /** {@code NUMVAL-C(text)} when {@link #isValid(String)}, else empty. */
    public static Optional<BigDecimal> parse(String text) {
        if (text == null) {
            return Optional.empty();
        }
        Matcher m = FORMAT.matcher(text);
        if (!m.matches()) {
            return Optional.empty();
        }
        String integer = m.group("int") == null ? "" : m.group("int").replace(",", "");
        String fraction = m.group("frac") == null ? "" : m.group("frac");
        if (integer.isEmpty() && fraction.isEmpty()) {
            return Optional.empty();
        }
        int signs = (m.group("lead1") == null ? 0 : 1) + (m.group("lead2") == null ? 0 : 1)
                + (m.group("trail") == null ? 0 : 1);
        if (signs > 1) {
            return Optional.empty();
        }
        String sign = m.group("lead1") != null ? m.group("lead1")
                : m.group("lead2") != null ? m.group("lead2") : m.group("trail");
        boolean negative = sign != null && (sign.equals("-") || sign.equalsIgnoreCase("CR")
                || sign.equalsIgnoreCase("DB"));
        BigDecimal value = new BigDecimal((integer.isEmpty() ? "0" : integer)
                + (fraction.isEmpty() ? "" : "." + fraction));
        return Optional.of(negative ? value.negate() : value);
    }
}
