package com.carddemo.common.cobol;

import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parsed PICTURE string.
 *
 * @param source        the picture as written, e.g. {@code S9(10)V99}
 * @param expanded      repetitions expanded, e.g. {@code S999999999999V99} -> {@code S9999999999V99}
 * @param category      alphanumeric, numeric or numeric-edited
 * @param digits        digit positions (numeric/numeric-edited)
 * @param scale         digits right of the (implied) decimal point
 * @param signed        {@code S} present (numeric) or a sign symbol present (edited)
 * @param displayLength characters occupied in USAGE DISPLAY (excluding a SEPARATE sign)
 */
public record PicClause(String source, String expanded, Category category, int digits, int scale, boolean signed,
                        int displayLength) {

    public enum Category { ALPHANUMERIC, NUMERIC, NUMERIC_EDITED }

    private static final Pattern REPEAT = Pattern.compile("(.)\\((\\d+)\\)");

    public static PicClause parse(String picture) {
        String pic = picture.toUpperCase(Locale.ROOT).trim();
        String expanded = expand(pic);
        if (expanded.matches("S?[9P]*V?[9P]*") && expanded.indexOf('9') >= 0) {
            if (expanded.indexOf('P') >= 0) {
                throw new CobolDataException("Scaling position P is not supported: " + picture);
            }
            boolean signed = expanded.startsWith("S");
            int v = expanded.indexOf('V');
            int digits = count(expanded, '9');
            int scale = v < 0 ? 0 : count(expanded.substring(v), '9');
            return new PicClause(picture, expanded, Category.NUMERIC, digits, scale, signed, digits);
        }
        if (expanded.matches("[XA9]+")) {
            return new PicClause(picture, expanded, Category.ALPHANUMERIC, 0, 0, false, expanded.length());
        }
        if (expanded.matches("[-+$Z9*,./B0V]*(CR|DB)?[-+]?") && !expanded.isEmpty()) {
            return editedNumeric(picture, expanded);
        }
        throw new CobolDataException("Unsupported PICTURE: " + picture);
    }

    public boolean isNumeric() {
        return category == Category.NUMERIC;
    }

    public boolean isAlphanumeric() {
        return category == Category.ALPHANUMERIC;
    }

    public boolean isEdited() {
        return category == Category.NUMERIC_EDITED;
    }

    private static PicClause editedNumeric(String picture, String expanded) {
        String body = expanded.replace("CR", "").replace("DB", "");
        int point = body.indexOf('.') >= 0 ? body.indexOf('.') : body.indexOf('V');
        char floating = floatingSymbol(body);
        int digits = 0;
        int scale = 0;
        boolean firstFloatSeen = false;
        for (int i = 0; i < body.length(); i++) {
            char c = body.charAt(i);
            boolean digit = c == '9' || c == 'Z' || c == '*';
            if (c == floating && floating != 0) {
                digit = firstFloatSeen;
                firstFloatSeen = true;
            }
            if (digit) {
                digits++;
                if (point >= 0 && i > point) {
                    scale++;
                }
            }
        }
        boolean signed = expanded.indexOf('+') >= 0 || expanded.indexOf('-') >= 0
                || expanded.endsWith("CR") || expanded.endsWith("DB");
        int length = expanded.length() - (expanded.indexOf('V') >= 0 ? 1 : 0);
        return new PicClause(picture, expanded, Category.NUMERIC_EDITED, digits, scale, signed, length);
    }

    /** The floating insertion symbol ($, + or -) if the picture starts with two or more of it, else 0. */
    static char floatingSymbol(String body) {
        for (char c : new char[] {'$', '+', '-'}) {
            int first = body.indexOf(c);
            if (first >= 0 && body.indexOf(c, first + 1) > first) {
                return c;
            }
        }
        return 0;
    }

    private static String expand(String pic) {
        Matcher m = REPEAT.matcher(pic);
        StringBuilder sb = new StringBuilder();
        while (m.find()) {
            m.appendReplacement(sb, Matcher.quoteReplacement(m.group(1).repeat(Integer.parseInt(m.group(2)))));
        }
        m.appendTail(sb);
        return sb.toString();
    }

    private static int count(String s, char c) {
        return (int) s.chars().filter(ch -> ch == c).count();
    }
}
