package com.carddemo.common;

import java.util.regex.Pattern;

/**
 * Primary account number masking (ADR-0020): list responses and log lines show only the last four digits of a card
 * number. The COBOL programs mask nothing; this is a deliberate PCI DSS 3.4 improvement.
 */
public final class PanMask {

    public static final int VISIBLE_DIGITS = 4;

    /** Card path segment that may be a typed card number (COCRDSLC/COCRDUPC accept a PAN as well as a cardRef). */
    private static final Pattern CARD_PATH_PAN = Pattern.compile("(?<=^/api/v1/cards/)\\d{13,19}(?=/|$)");

    private PanMask() {
    }

    /** {@code 0500024453765740} → {@code ************5740}; values of four characters or fewer are fully masked. */
    public static String mask(String pan) {
        if (pan == null) {
            return null;
        }
        if (pan.length() <= VISIBLE_DIGITS) {
            return "*".repeat(pan.length());
        }
        return "*".repeat(pan.length() - VISIBLE_DIGITS) + pan.substring(pan.length() - VISIBLE_DIGITS);
    }

    /**
     * A request path with any card number typed into {@code /api/v1/cards/<pan>} masked (s6.4), for the
     * {@code instance} of error bodies; other paths are returned unchanged.
     */
    public static String maskCardPath(String path) {
        if (path == null) {
            return null;
        }
        return CARD_PATH_PAN.matcher(path).replaceFirst(m -> mask(m.group()));
    }
}
