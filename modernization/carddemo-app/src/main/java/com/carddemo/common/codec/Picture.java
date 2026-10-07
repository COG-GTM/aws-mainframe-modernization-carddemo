package com.carddemo.common.codec;

import java.util.Locale;

/**
 * A parsed {@code PIC} character string.
 *
 * @param text     the expanded picture, e.g. {@code S9999999999V99} for {@code S9(10)V99}
 * @param category alphanumeric, numeric or numeric-edited
 * @param size     display positions (characters for DISPLAY usage)
 * @param digits   digit positions ({@code 9}, {@code Z}, {@code *})
 * @param scale    digit positions after {@code V} or the edited decimal point
 * @param signed   {@code S} present
 */
public record Picture(String text, Category category, int size, int digits, int scale, boolean signed) {

    public enum Category { ALPHABETIC, ALPHANUMERIC, NUMERIC, NUMERIC_EDITED }

    public static Picture parse(String picture) {
        String text = expand(picture);
        if (text.isEmpty()) {
            throw new RecordFormatException("empty PIC");
        }
        if (text.matches("S?9*V?9*") && text.indexOf('9') >= 0) {
            boolean signed = text.charAt(0) == 'S';
            int point = text.indexOf('V');
            int digits = count(text, '9');
            int scale = point < 0 ? 0 : count(text.substring(point), '9');
            return new Picture(text, Category.NUMERIC, digits, digits, scale, signed);
        }
        if (text.matches("A+")) {
            return new Picture(text, Category.ALPHABETIC, text.length(), 0, 0, false);
        }
        if (text.matches("[XA9]+")) {
            return new Picture(text, Category.ALPHANUMERIC, text.length(), 0, 0, false);
        }
        if (text.matches("[9Z*,.+\\-B0/$V]*(CR|DB)?") && text.matches(".*[9Z*].*")) {
            int point = Math.max(text.indexOf('.'), text.indexOf('V'));
            int digits = 0;
            int scale = 0;
            for (int i = 0; i < text.length(); i++) {
                char c = text.charAt(i);
                if (c == '9' || c == 'Z' || c == '*') {
                    digits++;
                    if (point >= 0 && i > point) {
                        scale++;
                    }
                }
            }
            int size = text.length() - count(text, 'V');
            return new Picture(text, Category.NUMERIC_EDITED, size, digits, scale,
                    text.indexOf('+') >= 0 || text.indexOf('-') >= 0 || text.endsWith("CR") || text.endsWith("DB"));
        }
        throw new RecordFormatException("unsupported PIC " + picture);
    }

    /** Expands repetition factors: {@code X(3)9(2)} becomes {@code XXX99}. */
    public static String expand(String picture) {
        String pic = picture.trim().toUpperCase(Locale.ROOT);
        StringBuilder out = new StringBuilder();
        for (int i = 0; i < pic.length(); i++) {
            char c = pic.charAt(i);
            if (c == '(') {
                int close = pic.indexOf(')', i);
                if (close < 0 || out.isEmpty()) {
                    throw new RecordFormatException("malformed PIC " + picture);
                }
                int repeat;
                try {
                    repeat = Integer.parseInt(pic.substring(i + 1, close).trim());
                } catch (NumberFormatException e) {
                    throw new RecordFormatException("malformed PIC " + picture, e);
                }
                if (repeat < 1) {
                    throw new RecordFormatException("malformed PIC " + picture);
                }
                out.append(String.valueOf(out.charAt(out.length() - 1)).repeat(repeat - 1));
                i = close;
            } else {
                out.append(c);
            }
        }
        return out.toString();
    }

    public boolean isNumeric() {
        return category == Category.NUMERIC;
    }

    private static int count(String s, char c) {
        return (int) s.chars().filter(ch -> ch == c).count();
    }
}
