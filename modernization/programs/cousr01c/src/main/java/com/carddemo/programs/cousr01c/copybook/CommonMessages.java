package com.carddemo.programs.cousr01c.copybook;

/** CSMSG01Y - CCDA-COMMON-MESSAGES. */
public final class CommonMessages {

    /** CCDA-MSG-THANK-YOU PIC X(50). */
    public static final String CCDA_MSG_THANK_YOU = Pic.x("Thank you for using CardDemo application...", 50);
    /** CCDA-MSG-INVALID-KEY PIC X(50). */
    public static final String CCDA_MSG_INVALID_KEY = Pic.x("Invalid key pressed. Please see below...", 50);

    private CommonMessages() {
    }
}
