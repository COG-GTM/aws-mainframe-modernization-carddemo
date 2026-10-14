package com.carddemo.runtime.cobol;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * MOVE of a numeric value to a numeric-edited item (e.g. BMS {@code PICOUT='+ZZZ,ZZZ,ZZZ.99'}). Supports the
 * symbols 9, Z, comma, period and a fixed leading or trailing + / - sign. Excess high-order integer digits
 * and low-order fraction digits are truncated, as in a COBOL MOVE.
 */
public final class NumericEdited {

    private NumericEdited() {
    }

    public static String format(BigDecimal value, String picture) {
        int point = picture.indexOf('.');
        String intPart = point < 0 ? picture : picture.substring(0, point);
        String fracPart = point < 0 ? "" : picture.substring(point + 1);
        int intDigits = countDigits(intPart);
        int fracDigits = countDigits(fracPart);
        BigDecimal abs = value.abs().setScale(fracDigits, RoundingMode.DOWN);
        String digits = abs.unscaledValue().toString();
        int total = intDigits + fracDigits;
        if (digits.length() > total) {
            digits = digits.substring(digits.length() - total);
        } else {
            digits = "0".repeat(total - digits.length()) + digits;
        }
        boolean negative = value.signum() < 0;
        boolean zero = digits.chars().allMatch(c -> c == '0');
        if (zero && picture.indexOf('9') < 0) {
            return PicX.spaces(picture.length());
        }
        StringBuilder out = new StringBuilder(picture.length());
        boolean suppressing = true;
        int d = 0;
        for (int i = 0; i < picture.length(); i++) {
            char p = picture.charAt(i);
            switch (p) {
                case '+' -> out.append(negative ? '-' : '+');
                case '-' -> out.append(negative ? '-' : ' ');
                case 'Z', 'z' -> {
                    char c = digits.charAt(d++);
                    if (suppressing && c == '0') {
                        out.append(' ');
                    } else {
                        suppressing = false;
                        out.append(c);
                    }
                }
                case '9' -> {
                    suppressing = false;
                    out.append(digits.charAt(d++));
                }
                case ',' -> out.append(suppressing ? ' ' : ',');
                case '.' -> {
                    suppressing = false;
                    out.append('.');
                }
                default -> throw new IllegalArgumentException("Unsupported edit symbol '" + p + "' in " + picture);
            }
        }
        return out.toString();
    }

    private static int countDigits(String picturePart) {
        int n = 0;
        for (char c : picturePart.toCharArray()) {
            if (c == '9' || c == 'Z' || c == 'z') {
                n++;
            }
        }
        return n;
    }
}
