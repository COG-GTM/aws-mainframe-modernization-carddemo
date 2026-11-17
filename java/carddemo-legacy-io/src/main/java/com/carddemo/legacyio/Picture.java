package com.carddemo.legacyio;

import java.util.Locale;

/**
 * A parsed PICTURE string.
 *
 * @param category      alphanumeric ({@code X}, {@code A}), numeric ({@code 9}, {@code S}, {@code V}) or numeric-edited
 * @param displayLength characters the item occupies with USAGE DISPLAY
 * @param digits        number of digit positions (numeric only)
 * @param scale         digits after the implied decimal point {@code V} (numeric only)
 * @param signed        whether the picture starts with {@code S} (numeric only)
 */
public record Picture(Category category, int displayLength, int digits, int scale, boolean signed) {

    public enum Category {
        ALPHANUMERIC,
        NUMERIC,
        /** Numeric-edited ({@code Z}, {@code -}, {@code .}, ...); stored and decoded as text. */
        EDITED
    }

    public static Picture parse(String picture) {
        String expanded = expand(picture.toUpperCase(Locale.ROOT));
        if (expanded.isEmpty()) {
            throw new IllegalArgumentException("empty PICTURE");
        }
        boolean numeric = expanded.matches("S?9*(V9*)?") && expanded.indexOf('9') >= 0;
        if (numeric) {
            boolean signed = expanded.charAt(0) == 'S';
            int point = expanded.indexOf('V');
            int digits = (int) expanded.chars().filter(c -> c == '9').count();
            int scale = point < 0 ? 0 : expanded.length() - point - 1;
            if (digits > 18) {
                throw new IllegalArgumentException("PICTURE " + picture + " has more than 18 digits");
            }
            return new Picture(Category.NUMERIC, digits, digits, scale, signed);
        }
        if (expanded.indexOf('P') >= 0 || expanded.indexOf('S') >= 0 || expanded.indexOf('V') >= 0) {
            throw new IllegalArgumentException("unsupported PICTURE " + picture);
        }
        if (expanded.matches("[XA9]+")) {
            return new Picture(Category.ALPHANUMERIC, expanded.length(), 0, 0, false);
        }
        if (expanded.matches("[9Z*+\\-.,B0/$X]*(CR|DB)?")) {
            return new Picture(Category.EDITED, expanded.length(), 0, 0, false);
        }
        throw new IllegalArgumentException("unsupported PICTURE " + picture);
    }

    public boolean isNumeric() {
        return category == Category.NUMERIC;
    }

    /** Bytes the item occupies with the given usage. */
    public int byteLength(Usage usage) {
        if (usage == Usage.DISPLAY) {
            return displayLength;
        }
        if (!isNumeric()) {
            throw new IllegalArgumentException(usage + " requires a numeric PICTURE");
        }
        if (usage == Usage.PACKED_DECIMAL) {
            return digits / 2 + 1;
        }
        return digits <= 4 ? 2 : digits <= 9 ? 4 : 8;
    }

    private static String expand(String picture) {
        StringBuilder out = new StringBuilder();
        int i = 0;
        while (i < picture.length()) {
            char c = picture.charAt(i);
            if (c == '(') {
                int close = picture.indexOf(')', i);
                if (close < 0 || out.isEmpty()) {
                    throw new IllegalArgumentException("malformed PICTURE " + picture);
                }
                int count = Integer.parseInt(picture.substring(i + 1, close).trim());
                char repeated = out.charAt(out.length() - 1);
                out.append(String.valueOf(repeated).repeat(count - 1));
                i = close + 1;
            } else {
                out.append(c);
                i++;
            }
        }
        return out.toString();
    }
}
