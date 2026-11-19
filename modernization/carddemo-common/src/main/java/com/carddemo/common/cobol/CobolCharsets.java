package com.carddemo.common.cobol;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

/** Record encodings of the CardDemo sample data (D-005): ASCII files as ISO-8859-1, mainframe files as EBCDIC CP037. */
public final class CobolCharsets {

    public static final Charset ASCII = StandardCharsets.ISO_8859_1;
    public static final Charset EBCDIC = Charset.forName("IBM037");

    private CobolCharsets() {
    }

    /** {@code ISO-8859-1}, {@code ASCII}, {@code IBM037} or {@code CP037}. */
    public static Charset forName(String name) {
        return switch (name.trim().toUpperCase()) {
            case "ASCII", "ISO-8859-1", "LATIN1" -> ASCII;
            case "CP037", "IBM037", "EBCDIC" -> EBCDIC;
            default -> throw new IllegalArgumentException("Unsupported record charset: " + name);
        };
    }
}
