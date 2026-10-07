package com.carddemo.common.codec;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * MOVE of a number to a numeric-edited item. Supports {@code 9}, {@code Z}, {@code ,}, {@code .}, {@code V},
 * insertion {@code B}/{@code 0}/{@code /}, a fixed leading or trailing {@code +}/{@code -}, and trailing
 * {@code CR}/{@code DB} — the editing used by the CardDemo reports (e.g. {@code -ZZZ,ZZZ,ZZZ.ZZ},
 * {@code Z(9).99-}, {@code +99999999.99}). Fraction digits are truncated (ADR-0005), excess high-order digits
 * dropped, and a zero value in a picture without {@code 9} becomes all spaces.
 */
public final class NumericEdited {

    private NumericEdited() {
    }

    public static String format(BigDecimal value, String picture) {
        Picture pic = Picture.parse(picture);
        if (pic.category() != Picture.Category.NUMERIC_EDITED) {
            throw new IllegalArgumentException("not a numeric-edited picture: " + picture);
        }
        String text = pic.text();
        BigDecimal v = value.setScale(pic.scale(), RoundingMode.DOWN);
        String digits = v.abs().unscaledValue().toString();
        if (digits.length() > pic.digits()) {
            digits = digits.substring(digits.length() - pic.digits());
        }
        digits = "0".repeat(pic.digits() - digits.length()) + digits;
        boolean negative = v.signum() < 0 && digits.chars().anyMatch(c -> c != '0');
        if (digits.chars().allMatch(c -> c == '0') && text.indexOf('9') < 0) {
            return " ".repeat(pic.size());
        }
        StringBuilder out = new StringBuilder(pic.size());
        boolean suppressing = true;
        int d = 0;
        for (int i = 0; i < text.length(); i++) {
            char c = text.charAt(i);
            switch (c) {
                case '9' -> {
                    suppressing = false;
                    out.append(digits.charAt(d++));
                }
                case 'Z', '*' -> {
                    char digit = digits.charAt(d++);
                    if (suppressing && digit == '0') {
                        out.append(c == '*' ? '*' : ' ');
                    } else {
                        suppressing = false;
                        out.append(digit);
                    }
                }
                case ',', '0', '/' -> out.append(suppressing ? ' ' : c);
                case 'B' -> out.append(' ');
                case '.' -> {
                    suppressing = false;
                    out.append('.');
                }
                case 'V' -> suppressing = false;
                case '+' -> out.append(negative ? '-' : '+');
                case '-' -> out.append(negative ? '-' : ' ');
                case 'C', 'D' -> {
                    out.append(negative ? text.substring(i, i + 2) : "  ");
                    i++;
                }
                default -> out.append(c);
            }
        }
        return out.toString();
    }
}
