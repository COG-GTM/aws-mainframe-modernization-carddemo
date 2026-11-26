package com.carddemo.programs.cbtrn03c.support;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * MOVE to a numeric-edited item whose picture is a fixed sign ('+' or '-') followed by Z, 9, ',' and '.',
 * e.g. {@code -ZZZ,ZZZ,ZZZ.ZZ} and {@code +ZZZ,ZZZ,ZZZ.ZZ} from CVTRA07Y.
 */
public final class PicEditor {
    private PicEditor() {
    }

    public static String edit(BigDecimal value, String picture) {
        int point = picture.indexOf('.');
        String intPart = point < 0 ? picture : picture.substring(0, point);
        String fracPart = point < 0 ? "" : picture.substring(point + 1);
        int intDigits = countDigits(intPart);
        int fracDigits = countDigits(fracPart);
        BigDecimal v = CobolNumeric.truncate(value, intDigits, fracDigits);
        boolean negative = v.signum() < 0;
        boolean allZ = picture.chars().noneMatch(c -> c == '9');
        if (v.signum() == 0 && allZ) {
            return Fixed.spaces(picture.length());
        }
        String digits = v.abs().movePointRight(fracDigits).setScale(0, RoundingMode.DOWN).toBigInteger().toString();
        digits = "0".repeat(intDigits + fracDigits - digits.length()) + digits;

        StringBuilder out = new StringBuilder(picture.length());
        boolean significant = false;
        int d = 0;
        for (int i = 0; i < picture.length(); i++) {
            char p = picture.charAt(i);
            switch (p) {
                case '+' -> out.append(negative ? '-' : '+');
                case '-' -> out.append(negative ? '-' : ' ');
                case 'Z' -> {
                    char c = digits.charAt(d++);
                    if (significant || c != '0') {
                        significant = true;
                        out.append(c);
                    } else {
                        out.append(' ');
                    }
                }
                case '9' -> {
                    significant = true;
                    out.append(digits.charAt(d++));
                }
                case ',' -> out.append(significant ? ',' : ' ');
                case '.' -> {
                    significant = true;
                    out.append('.');
                }
                default -> throw new IllegalArgumentException("Unsupported picture symbol '" + p + "' in " + picture);
            }
        }
        return out.toString();
    }

    private static int countDigits(String picture) {
        int n = 0;
        for (int i = 0; i < picture.length(); i++) {
            char c = picture.charAt(i);
            if (c == 'Z' || c == '9') {
                n++;
            }
        }
        return n;
    }
}
