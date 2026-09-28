package com.carddemo.batch.record;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * COBOL numeric-edited MOVE for the pictures used by the reports: {@code 9}, {@code Z}, {@code ,}, {@code .}
 * and a single fixed leading or trailing {@code +}/{@code -}. Pictures are given expanded, e.g.
 * {@code "ZZZZZZZZZ.99-"} for {@code Z(9).99-}.
 */
public final class Edited {

    private Edited() {
    }

    public static String format(BigDecimal value, String picture) {
        int point = picture.indexOf('.');
        int scale = 0;
        int digitCount = 0;
        for (int i = 0; i < picture.length(); i++) {
            char c = picture.charAt(i);
            if (c == '9' || c == 'Z') {
                digitCount++;
                if (point >= 0 && i > point) {
                    scale++;
                }
            }
        }
        BigDecimal v = value.setScale(scale, RoundingMode.DOWN);
        String digits = v.abs().unscaledValue().toString();
        if (digits.length() > digitCount) {
            digits = digits.substring(digits.length() - digitCount);
        }
        digits = "0".repeat(digitCount - digits.length()) + digits;
        boolean negative = v.signum() < 0;
        if (v.signum() == 0 && picture.indexOf('9') < 0) {
            return " ".repeat(picture.length());
        }
        StringBuilder out = new StringBuilder(picture.length());
        boolean suppressing = true;
        int d = 0;
        for (int i = 0; i < picture.length(); i++) {
            char c = picture.charAt(i);
            switch (c) {
                case '9' -> {
                    suppressing = false;
                    out.append(digits.charAt(d++));
                }
                case 'Z' -> {
                    char digit = digits.charAt(d++);
                    if (suppressing && digit == '0') {
                        out.append(' ');
                    } else {
                        suppressing = false;
                        out.append(digit);
                    }
                }
                case ',' -> out.append(suppressing ? ' ' : ',');
                case '.' -> {
                    suppressing = false;
                    out.append('.');
                }
                case '+' -> out.append(negative ? '-' : '+');
                case '-' -> out.append(negative ? '-' : ' ');
                default -> out.append(c);
            }
        }
        return out.toString();
    }
}
