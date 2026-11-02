package com.carddemo.common.record;

import java.util.Locale;

/** Parses the subset of PICTURE / USAGE clauses used by the CardDemo record copybooks. */
record Picture(String text, boolean alphanumeric, int digits, int scale, boolean signed, Usage usage, int length) {

    static Picture parse(String clause) {
        String normalized = clause.trim().toUpperCase(Locale.ROOT);
        String[] parts = normalized.split("\\s+");
        if (parts.length > 2) {
            throw new IllegalArgumentException("Unsupported picture clause " + clause);
        }
        String pic = parts[0];
        Usage usage = parts.length == 2 ? Usage.parse(parts[1]) : Usage.DISPLAY;

        int i = 0;
        boolean signed = false;
        if (pic.startsWith("S")) {
            signed = true;
            i = 1;
        }
        int alpha = 0;
        int integerDigits = 0;
        int decimals = 0;
        boolean afterV = false;
        while (i < pic.length()) {
            char symbol = pic.charAt(i++);
            if (symbol == 'V') {
                if (afterV) {
                    throw new IllegalArgumentException("Two V in picture " + clause);
                }
                afterV = true;
                continue;
            }
            int count = 1;
            if (i < pic.length() && pic.charAt(i) == '(') {
                int close = pic.indexOf(')', i);
                if (close < 0) {
                    throw new IllegalArgumentException("Unbalanced picture " + clause);
                }
                count = Integer.parseInt(pic.substring(i + 1, close));
                i = close + 1;
            }
            switch (symbol) {
                case 'X' -> alpha += count;
                case '9' -> {
                    if (afterV) {
                        decimals += count;
                    } else {
                        integerDigits += count;
                    }
                }
                default -> throw new IllegalArgumentException("Unsupported picture symbol '" + symbol + "' in " + clause);
            }
        }

        if (alpha > 0) {
            if (integerDigits + decimals > 0 || signed || afterV || usage != Usage.DISPLAY) {
                throw new IllegalArgumentException("Mixed alphanumeric picture " + clause);
            }
            return new Picture(normalized, true, 0, 0, false, Usage.DISPLAY, alpha);
        }
        int digits = integerDigits + decimals;
        if (digits == 0 || digits > 18) {
            throw new IllegalArgumentException("Numeric picture must have 1-18 digits: " + clause);
        }
        int length = switch (usage) {
            case DISPLAY -> digits;
            case PACKED -> digits / 2 + 1;
            case BINARY -> digits <= 4 ? 2 : digits <= 9 ? 4 : 8;
        };
        return new Picture(normalized, false, digits, decimals, signed, usage, length);
    }
}
