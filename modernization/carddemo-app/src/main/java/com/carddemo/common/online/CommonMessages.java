package com.carddemo.common.online;

/**
 * {@code CSMSG01Y} / {@code COTTL01Y} texts, right-trimmed (the PIC X fields are space padded; ADR-0003).
 */
public final class CommonMessages {

    /** {@code CCDA-MSG-THANK-YOU}: sent on PF3 from the sign-on screen. */
    public static final String THANK_YOU = "Thank you for using CardDemo application...";

    /** {@code CCDA-MSG-INVALID-KEY}: any AID a program does not handle. */
    public static final String INVALID_KEY = "Invalid key pressed. Please see below...";

    /** {@code CCDA-TITLE01}. */
    public static final String TITLE01 = "AWS Mainframe Modernization";

    /** {@code CCDA-TITLE02}. */
    public static final String TITLE02 = "CardDemo";

    private CommonMessages() {
    }
}
