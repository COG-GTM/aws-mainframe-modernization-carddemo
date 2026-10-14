package com.carddemo.runtime.cobol;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

/**
 * MOVE of a numeric value to a numeric-edited item ({@code PIC +ZZZ,ZZZ,ZZZ.99}, {@code -ZZ9.99},
 * {@code $$$,$$9.99CR} ...). Supported symbols: 9, Z, *, fixed or floating + - $, CR/DB, ',' '.' B 0 '/'.
 * High-order digits that do not fit are truncated, excess decimals are truncated (MOVE rules).
 */
public final class NumericEdited {

    private NumericEdited() {
    }

    public static String format(BigDecimal value, String picture) {
        String pic = expand(picture);
        String trailer = "";
        if (pic.endsWith("CR") || pic.endsWith("DB")) {
            trailer = pic.substring(pic.length() - 2);
            pic = pic.substring(0, pic.length() - 2);
        }
        char first = pic.isEmpty() ? ' ' : pic.charAt(0);
        boolean floating = (first == '+' || first == '-' || first == '$') && countFloat(pic, first) > 1;
        char fixedLead = 0;
        char fixedTrail = 0;
        if (!floating && (first == '+' || first == '-')) {
            fixedLead = first;
            pic = pic.substring(1);
        } else if (!pic.isEmpty() && (pic.endsWith("+") || pic.endsWith("-")) && !floating) {
            fixedTrail = pic.charAt(pic.length() - 1);
            pic = pic.substring(0, pic.length() - 1);
        }
        char fixedCurrency = 0;
        if (!floating && pic.startsWith("$")) {
            fixedCurrency = '$';
            pic = pic.substring(1);
        }
        int intSlots = 0;
        int fracSlots = 0;
        boolean afterPoint = false;
        boolean firstFloatSeen = false;
        for (char c : pic.toCharArray()) {
            boolean digitSlot = c == '9' || c == 'Z' || c == '*' || (floating && c == first && firstFloatSeen);
            if (floating && c == first) {
                firstFloatSeen = true;
            }
            if (c == '.') {
                afterPoint = true;
            } else if (digitSlot) {
                if (afterPoint) {
                    fracSlots++;
                } else {
                    intSlots++;
                }
            }
        }
        boolean negative = value.signum() < 0;
        BigInteger unscaled = value.abs().setScale(fracSlots, RoundingMode.DOWN).unscaledValue()
                .mod(BigInteger.TEN.pow(intSlots + fracSlots));
        String digits = String.format("%0" + Math.max(1, intSlots + fracSlots) + "d", unscaled);
        if (intSlots + fracSlots == 0) {
            digits = "";
        }
        boolean zero = unscaled.signum() == 0;
        boolean hasNines = pic.indexOf('9') >= 0;
        boolean asterisk = pic.indexOf('*') >= 0;
        char fill = asterisk ? '*' : ' ';

        StringBuilder out = new StringBuilder();
        int d = 0;
        boolean suppress = true;
        int lastSuppressed = -1;
        firstFloatSeen = false;
        for (char c : pic.toCharArray()) {
            if (floating && c == first && !firstFloatSeen) {
                firstFloatSeen = true;
                out.append(' ');
                lastSuppressed = out.length() - 1;
                continue;
            }
            switch (c) {
                case '9' -> {
                    suppress = false;
                    out.append(digits.charAt(d++));
                }
                case 'Z', '*' -> appendSuppressible(out, digits.charAt(d++), suppress, fill);
                case '.' -> {
                    if (suppress && zero && !hasNines) {
                        out.append(fill);
                    } else {
                        suppress = false;
                        out.append('.');
                    }
                    continue;
                }
                case ',', 'B', '0', '/' -> {
                    if (suppress) {
                        out.append(floating ? ' ' : fill);
                        if (floating) {
                            lastSuppressed = out.length() - 1;
                        }
                    } else {
                        out.append(c == 'B' ? ' ' : c);
                    }
                    continue;
                }
                default -> {
                    if (floating && c == first) {
                        char dc = digits.charAt(d++);
                        if (suppress && dc == '0') {
                            out.append(' ');
                            lastSuppressed = out.length() - 1;
                        } else {
                            suppress = false;
                            out.append(dc);
                        }
                    } else {
                        out.append(c);
                    }
                    continue;
                }
            }
            if (c == 'Z' || c == '*') {
                if (out.charAt(out.length() - 1) != fill || !suppress) {
                    suppress = false;
                }
            }
        }
        if (floating && lastSuppressed >= 0) {
            char sym = first == '$' ? '$' : (negative && !zero ? '-' : (first == '+' ? '+' : ' '));
            out.setCharAt(lastSuppressed, sym);
        }
        String body = out.toString();
        if (zero && !hasNines && !floating && pic.indexOf('Z') >= 0 && !asterisk) {
            body = PicX.spaces(body.length());
            String lead = fixedLead == 0 ? "" : " ";
            String trail = fixedTrail == 0 ? "" : " ";
            return lead + (fixedCurrency == 0 ? "" : " ") + body + trail + PicX.spaces(trailer.length());
        }
        StringBuilder result = new StringBuilder();
        if (fixedLead != 0) {
            result.append(negative && !zero ? '-' : (fixedLead == '+' ? '+' : ' '));
        }
        if (fixedCurrency != 0) {
            result.append('$');
        }
        result.append(body);
        if (fixedTrail != 0) {
            result.append(negative && !zero ? '-' : (fixedTrail == '+' ? '+' : ' '));
        }
        if (!trailer.isEmpty()) {
            result.append(negative && !zero ? trailer : "  ");
        }
        return result.toString();
    }

    private static void appendSuppressible(StringBuilder out, char digit, boolean suppress, char fill) {
        out.append(suppress && digit == '0' ? fill : digit);
    }

    private static int countFloat(String pic, char symbol) {
        int n = 0;
        for (char c : pic.toCharArray()) {
            if (c == symbol) {
                n++;
            } else if (c != ',' && c != 'B' && c != '0' && c != '/') {
                break;
            }
        }
        return n;
    }

    /** Expands repetition factors: {@code Z(3)9.9(2)} -> {@code ZZZ9.99}. */
    static String expand(String picture) {
        StringBuilder sb = new StringBuilder();
        String p = picture.toUpperCase();
        for (int i = 0; i < p.length(); i++) {
            char c = p.charAt(i);
            if (c == '(') {
                int close = p.indexOf(')', i);
                int count = Integer.parseInt(p.substring(i + 1, close));
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
