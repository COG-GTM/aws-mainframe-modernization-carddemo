package com.carddemo.common.cobol;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * Formats and de-edits numeric-edited pictures (report fields such as {@code -ZZZ,ZZZ,ZZZ.ZZ} in CVTRA07Y).
 * Supports 9, Z, *, the insertion characters {@code , . / B 0}, an implied {@code V}, fixed or floating
 * {@code + - $}, and trailing {@code CR}/{@code DB}.
 */
public final class NumericEdited {

    private NumericEdited() {
    }

    public static String format(BigDecimal value, String picture) {
        return format(value, PicClause.parse(picture));
    }

    public static String format(BigDecimal value, PicClause pic) {
        if (!pic.isEdited()) {
            throw new CobolDataException("Not a numeric-edited picture: " + pic.source());
        }
        String p = pic.expanded();
        String suffix = "";
        if (p.endsWith("CR") || p.endsWith("DB")) {
            suffix = p.substring(p.length() - 2);
            p = p.substring(0, p.length() - 2);
        }
        BigDecimal scaled = value.setScale(pic.scale(), RoundingMode.DOWN);
        boolean negative = scaled.signum() < 0;
        String digits = scaled.unscaledValue().abs().toString();
        if (digits.length() > pic.digits()) {
            digits = digits.substring(digits.length() - pic.digits()); // COBOL truncates high-order digits
        }
        digits = "0".repeat(pic.digits() - digits.length()) + digits;
        boolean zero = scaled.signum() == 0;

        char floating = PicClause.floatingSymbol(p);
        int fixedSignAt = -1;
        if (floating != '+' && floating != '-') {
            if (p.startsWith("+") || p.startsWith("-")) {
                fixedSignAt = 0;
            } else if (p.endsWith("+") || p.endsWith("-")) {
                fixedSignAt = p.length() - 1;
            }
        }
        boolean check = p.indexOf('*') >= 0;
        char fill = check ? '*' : ' ';

        StringBuilder out = new StringBuilder();
        int d = 0;
        boolean significant = false;
        boolean firstFloat = true;
        int pendingFloat = -1;
        for (int i = 0; i < p.length(); i++) {
            char c = p.charAt(i);
            if (i == fixedSignAt) {
                out.append(c == '+' ? (negative ? '-' : '+') : (negative ? '-' : ' '));
            } else if (c == floating && floating != 0) {
                if (firstFloat) {
                    firstFloat = false;
                    pendingFloat = out.length();
                    out.append(' ');
                } else {
                    char digit = digits.charAt(d++);
                    if (!significant && digit == '0') {
                        pendingFloat = out.length();
                        out.append(' ');
                    } else {
                        significant = placeFloat(out, pendingFloat, floating, negative, significant);
                        out.append(digit);
                    }
                }
            } else {
                switch (c) {
                    case '9' -> {
                        significant = placeFloat(out, pendingFloat, floating, negative, significant);
                        out.append(digits.charAt(d++));
                    }
                    case 'Z', '*' -> {
                        char digit = digits.charAt(d++);
                        if (!significant && digit == '0') {
                            out.append(c == '*' ? '*' : ' ');
                        } else {
                            significant = true;
                            out.append(digit);
                        }
                    }
                    case ',', '/', 'B', '0' -> {
                        if (significant) {
                            out.append(c == 'B' ? ' ' : c);
                        } else {
                            if (floating != 0 && pendingFloat >= 0) {
                                pendingFloat = out.length();
                            }
                            out.append(fill);
                        }
                    }
                    case '.', 'V' -> {
                        significant = placeFloat(out, pendingFloat, floating, negative, significant);
                        if (c == '.') {
                            out.append('.');
                        }
                    }
                    case '$' -> out.append('$');
                    default -> throw new CobolDataException("Unexpected picture symbol '" + c + "' in " + pic.source());
                }
            }
        }
        if (!suffix.isEmpty()) {
            out.append(negative ? suffix : "  ");
        }
        if (zero && p.indexOf('9') < 0) {
            if (check) {
                return out.toString().replaceAll("[0-9]", "*");
            }
            return " ".repeat(out.length());
        }
        return out.toString();
    }

    private static boolean placeFloat(StringBuilder out, int pendingFloat, char floating, boolean negative,
                                      boolean significant) {
        if (!significant && floating != 0 && pendingFloat >= 0) {
            char symbol = switch (floating) {
                case '+' -> negative ? '-' : '+';
                case '-' -> negative ? '-' : ' ';
                default -> '$';
            };
            out.setCharAt(pendingFloat, symbol);
        }
        return true;
    }

    /** De-edits a formatted value back to a number (blank -> zero). */
    public static BigDecimal parse(String text, PicClause pic) {
        String positions = pic.expanded().replace("V", "");
        if (text.length() == positions.length() && positions.indexOf('0') >= 0) {
            StringBuilder kept = new StringBuilder();
            for (int i = 0; i < text.length(); i++) {
                if (positions.charAt(i) != '0') {
                    kept.append(text.charAt(i));
                }
            }
            text = kept.toString();
        }
        String trimmed = text.trim();
        boolean negative = trimmed.contains("-") || trimmed.endsWith("CR") || trimmed.endsWith("DB");
        String digits = trimmed.replaceAll("[^0-9]", "");
        if (digits.isEmpty()) {
            return BigDecimal.ZERO.setScale(pic.scale());
        }
        BigDecimal value = new BigDecimal(new java.math.BigInteger(digits), pic.scale());
        return negative ? value.negate() : value;
    }
}
