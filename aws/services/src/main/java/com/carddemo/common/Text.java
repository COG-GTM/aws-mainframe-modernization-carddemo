package com.carddemo.common;

import java.util.Locale;

public final class Text {

    private Text() {
    }

    public static boolean isBlank(String value) {
        return value == null || value.isBlank();
    }

    public static String trim(String value) {
        return value == null ? null : value.strip();
    }

    public static String trimToEmpty(String value) {
        return value == null ? "" : value.strip();
    }

    public static String upperTrim(String value) {
        return value == null ? "" : value.strip().toUpperCase(Locale.ROOT);
    }

    public static boolean isDigits(String value) {
        return value != null && !value.isEmpty() && value.chars().allMatch(c -> c >= '0' && c <= '9');
    }

    public static boolean isAllZero(String value) {
        return value != null && !value.isEmpty() && value.chars().allMatch(c -> c == '0');
    }

    public static boolean isAlphaOrSpace(String value) {
        return value != null
                && value.chars().allMatch(c -> c == ' ' || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'));
    }

    public static String leftPadZeros(String digits, int length) {
        return "0".repeat(Math.max(0, length - digits.length())) + digits;
    }
}
