package com.carddemo.account.online;

import com.carddemo.common.online.ScreenInput;

final class Fields {

    private Fields() {
    }

    /** The {@code PIC X(width)} image of a value: space padded or cut. */
    static String fit(String value, int width) {
        String v = value == null ? "" : value;
        return v.length() >= width ? v.substring(0, width) : v + " ".repeat(width - v.length());
    }

    static String rightTrim(String value) {
        return value == null ? "" : ScreenInput.rightTrim(value);
    }

    /** {@code FUNCTION UPPER-CASE(FUNCTION TRIM(x))}. */
    static String upperTrim(String value) {
        return ScreenInput.upperCase(value == null ? "" : value.strip());
    }
}
