package com.carddemo.legacyio.copybook;

import java.util.Locale;

/**
 * A parsed {@code PIC} string.
 *
 * @param source the picture string as written, e.g. {@code S9(10)V99}
 * @param category what the item holds
 * @param size number of character positions (DISPLAY length); for numerics the digit count
 * @param scale digits after the implied decimal point ({@code V})
 * @param signed whether the picture starts with {@code S}
 */
public record Picture(String source, Category category, int size, int scale, boolean signed) {

    /** The kind of data a picture describes. */
    public enum Category {
        /** {@code X} and {@code A} pictures. */
        ALPHANUMERIC,
        /** {@code 9}, {@code S}, {@code V} pictures. */
        NUMERIC,
        /** Pictures with editing symbols ({@code Z . , - + $ B / 0 *}); stored as characters. */
        EDITED
    }

    /** Digits of a numeric picture (integer plus fraction). */
    public int digits() {
        return size;
    }

    /** Parses a picture string such as {@code X(10)}, {@code 9(04)}, {@code S9(09)V99} or {@code ZZ9.99-}. */
    public static Picture parse(String source) {
        String pic = source.toUpperCase(Locale.ROOT);
        StringBuilder expanded = new StringBuilder();
        for (int i = 0; i < pic.length(); i++) {
            char c = pic.charAt(i);
            if (c == '(') {
                int close = pic.indexOf(')', i);
                if (close < 0 || expanded.isEmpty()) {
                    throw new CopybookParseException("Malformed PIC " + source);
                }
                int count;
                try {
                    count = Integer.parseInt(pic.substring(i + 1, close).trim());
                } catch (NumberFormatException e) {
                    throw new CopybookParseException("Malformed repeat count in PIC " + source);
                }
                char repeated = expanded.charAt(expanded.length() - 1);
                expanded.append(String.valueOf(repeated).repeat(count - 1));
                i = close;
            } else {
                expanded.append(c);
            }
        }
        String symbols = expanded.toString();
        if (symbols.isEmpty()) {
            throw new CopybookParseException("Empty PIC");
        }
        if (symbols.chars().allMatch(c -> c == 'X' || c == 'A' || c == '9') && symbols.chars().anyMatch(c -> c != '9')) {
            return new Picture(source, Category.ALPHANUMERIC, symbols.length(), 0, false);
        }
        if (symbols.matches("S?9*(V9*)?") && symbols.contains("9")) {
            boolean signed = symbols.startsWith("S");
            String digits = signed ? symbols.substring(1) : symbols;
            int v = digits.indexOf('V');
            int scale = v < 0 ? 0 : digits.length() - v - 1;
            int size = digits.replace("V", "").length();
            return new Picture(source, Category.NUMERIC, size, scale, signed);
        }
        if (symbols.matches("[9ZXAB0/,.+\\-*$CRDBV]+")) {
            int size = symbols.replace("V", "").length();
            return new Picture(source, Category.EDITED, size, 0, false);
        }
        throw new CopybookParseException("Unsupported PIC " + source);
    }

    /** Bytes the item occupies with the given usage. */
    public int byteLength(Usage usage) {
        return switch (usage) {
            case DISPLAY -> size;
            case PACKED_DECIMAL -> {
                requireNumeric(usage);
                yield size / 2 + 1;
            }
            case BINARY -> {
                requireNumeric(usage);
                if (size <= 4) {
                    yield 2;
                }
                if (size <= 9) {
                    yield 4;
                }
                if (size <= 18) {
                    yield 8;
                }
                throw new CopybookParseException("Binary PIC " + source + " has more than 18 digits");
            }
        };
    }

    private void requireNumeric(Usage usage) {
        if (category != Category.NUMERIC) {
            throw new CopybookParseException("USAGE " + usage + " needs a numeric PIC, got " + source);
        }
    }
}
