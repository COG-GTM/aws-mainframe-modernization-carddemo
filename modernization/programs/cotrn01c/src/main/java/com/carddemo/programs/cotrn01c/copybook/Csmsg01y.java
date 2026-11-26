package com.carddemo.programs.cotrn01c.copybook;

/** CSMSG01Y - 01 CCDA-COMMON-MESSAGES. Each message is PIC X(50); the VALUE literals are space padded to 50. */
public final class Csmsg01y {

    /** CCDA-MSG-THANK-YOU PIC X(50). */
    public static final String CCDA_MSG_THANK_YOU =
            Cobol.moveAlnum("Thank you for using CardDemo application...      ", 50);
    /** CCDA-MSG-INVALID-KEY PIC X(50). */
    public static final String CCDA_MSG_INVALID_KEY =
            Cobol.moveAlnum("Invalid key pressed. Please see below...         ", 50);

    private Csmsg01y() {
    }
}
