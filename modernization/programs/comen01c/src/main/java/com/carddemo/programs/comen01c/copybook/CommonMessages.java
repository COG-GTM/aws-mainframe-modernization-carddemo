package com.carddemo.programs.comen01c.copybook;

/** CSMSG01Y - {@code 01 CCDA-COMMON-MESSAGES}. */
public final class CommonMessages {

    /** CCDA-MSG-THANK-YOU PIC X(50). */
    public static final String CCDA_MSG_THANK_YOU =
            CobolText.move("Thank you for using CardDemo application...", 50);
    /** CCDA-MSG-INVALID-KEY PIC X(50). */
    public static final String CCDA_MSG_INVALID_KEY =
            CobolText.move("Invalid key pressed. Please see below...", 50);

    private CommonMessages() {
    }
}
