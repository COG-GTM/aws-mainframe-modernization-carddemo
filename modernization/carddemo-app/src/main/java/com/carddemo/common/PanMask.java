package com.carddemo.common;

/**
 * Primary account number masking (ADR-0020): list responses and log lines show only the last four digits of a card
 * number. The COBOL programs mask nothing; this is a deliberate PCI DSS 3.4 improvement.
 */
public final class PanMask {

    public static final int VISIBLE_DIGITS = 4;

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
}
