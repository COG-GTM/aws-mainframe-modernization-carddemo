package com.carddemo.programs.cosgn00c.copybook;

/** CSMSG01Y - CCDA-COMMON-MESSAGES. */
public final class CommonMessages {

    /** CCDA-MSG-THANK-YOU PIC X(50). */
    public static final String CCDA_MSG_THANK_YOU = CobolText.picX("Thank you for using CardDemo application...", 50);
    /** CCDA-MSG-INVALID-KEY PIC X(50). */
    public static final String CCDA_MSG_INVALID_KEY = CobolText.picX("Invalid key pressed. Please see below...", 50);

    private CommonMessages() {
    }
}
