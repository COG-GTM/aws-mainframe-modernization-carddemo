package com.carddemo.runtime.cobol;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

/**
 * MOVE of a numeric value to a numeric-edited item (e.g. {@code PIC +ZZZ,ZZZ,ZZZ.99}). Supported symbols:
 * {@code 9}, {@code Z}, {@code .} (decimal point), insertion {@code , B 0 /}, fixed {@code +}/{@code -} as the
 * first or last symbol, and floating {@code ++...}/{@code --...} strings; {@code n(k)} repetition is expanded.
 *
 * <p>As in a COBOL MOVE the value is truncated (not rounded) on both sides: excess fraction digits and
 * high-order integer digits that do not fit the picture are dropped. When every digit position is a
 * suppression symbol and the value is zero the whole item is spaces.
 */
public final class NumericEdited {

    private NumericEdited() {
    }

    public static String format(BigDecimal value, String picture) {
        char[] pic = expand(picture).toCharArray();
        int n = pic.length;
        boolean floating = n >= 2 && (pic[0] == '+' || pic[0] == '-') && pic[1] == pic[0];
        char floatChar = floating ? pic[0] : 0;
        boolean fixedLead = !floating && (pic[0] == '+' || pic[0] == '-');
        boolean fixedTrail = (pic[n - 1] == '+' || pic[n - 1] == '-') && !(floating && allFloating(pic, floatChar));

        int pointIdx = -1;
        for (int i = 0; i < n; i++) {
            if (pic[i] == '.') {
                pointIdx = i;
                break;
            }
        }
        int intDigits = 0;
        int fracDigits = 0;
        boolean allSuppress = true;
        for (int i = 0; i < n; i++) {
            if (isDigitPosition(pic, i, floating, floatChar, fixedLead, fixedTrail)) {
                if (pointIdx >= 0 && i > pointIdx) {
                    fracDigits++;
                } else {
                    intDigits++;
                }
                if (pic[i] == '9') {
                    allSuppress = false;
                }
            }
        }

        BigDecimal truncated = value.setScale(fracDigits, RoundingMode.DOWN);
        BigInteger unscaled = truncated.unscaledValue().abs().mod(BigInteger.TEN.pow(intDigits + fracDigits));
        boolean negative = truncated.signum() < 0 && unscaled.signum() != 0;
        boolean zero = unscaled.signum() == 0;
        if (zero && allSuppress) {
            return PicX.spaces(n);
        }
        String digits = String.format("%0" + (intDigits + fracDigits) + "d", unscaled);

        char[] out = new char[n];
        int d = 0;
        boolean significant = false;
        int lastSuppressedFloat = floating ? 0 : -1;
        for (int i = 0; i < n; i++) {
            char c = pic[i];
            if ((i == 0 && fixedLead) || (i == n - 1 && fixedTrail)) {
                out[i] = c == '+' ? (negative ? '-' : '+') : (negative ? '-' : ' ');
            } else if (floating && i == 0) {
                out[i] = ' ';
            } else if (isDigitPosition(pic, i, floating, floatChar, fixedLead, fixedTrail)) {
                char digit = digits.charAt(d++);
                if (c == '9' || significant || digit != '0') {
                    significant = true;
                    out[i] = digit;
                } else {
                    out[i] = ' ';
                    if (c == floatChar) {
                        lastSuppressedFloat = i;
                    }
                }
            } else if (c == '.') {
                out[i] = '.';
                significant = true;
            } else if (c == ',' || c == 'B' || c == '0' || c == '/') {
                if (significant) {
                    out[i] = c == 'B' ? ' ' : c;
                } else {
                    out[i] = ' ';
                    if (floating && i < firstNonFloat(pic, floatChar)) {
                        lastSuppressedFloat = i;
                    }
                }
            } else {
                out[i] = c;
            }
        }
        if (floating) {
            out[lastSuppressedFloat] = floatChar == '+' ? (negative ? '-' : '+') : (negative ? '-' : ' ');
        }
        return new String(out);
    }

    private static boolean isDigitPosition(char[] pic, int i, boolean floating, char floatChar, boolean fixedLead,
                                           boolean fixedTrail) {
        char c = pic[i];
        if ((i == 0 && fixedLead) || (i == pic.length - 1 && fixedTrail)) {
            return false;
        }
        if (c == '9' || c == 'Z') {
            return true;
        }
        return floating && c == floatChar && i > 0;
    }

    private static boolean allFloating(char[] pic, char floatChar) {
        int count = 0;
        for (char c : pic) {
            if (c == floatChar) {
                count++;
            }
        }
        return count >= 2 && pic[pic.length - 1] == floatChar && firstNonFloat(pic, floatChar) == pic.length;
    }

    private static int firstNonFloat(char[] pic, char floatChar) {
        for (int i = 0; i < pic.length; i++) {
            char c = pic[i];
            if (c != floatChar && c != ',' && c != 'B' && c != '0' && c != '/') {
                return i;
            }
        }
        return pic.length;
    }

    static String expand(String picture) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < picture.length(); i++) {
            char c = picture.charAt(i);
            if (c == '(') {
                int close = picture.indexOf(')', i);
                int count = Integer.parseInt(picture.substring(i + 1, close));
                char prev = sb.charAt(sb.length() - 1);
                sb.append(String.valueOf(prev).repeat(count - 1));
                i = close;
            } else {
                sb.append(c);
            }
        }
        return sb.toString();
    }
}
