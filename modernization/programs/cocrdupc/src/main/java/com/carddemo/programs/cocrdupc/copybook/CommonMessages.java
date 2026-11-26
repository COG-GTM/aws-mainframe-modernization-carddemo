package com.carddemo.programs.cocrdupc.copybook;

import com.carddemo.programs.cocrdupc.Cobol;

/** CSMSG01Y: 01 CCDA-COMMON-MESSAGES. */
public final class CommonMessages {
    /** CCDA-MSG-THANK-YOU PIC X(50). */
    public static final String CCDA_MSG_THANK_YOU = Cobol.alnum("Thank you for using CardDemo application...", 50);
    /** CCDA-MSG-INVALID-KEY PIC X(50). */
    public static final String CCDA_MSG_INVALID_KEY = Cobol.alnum("Invalid key pressed. Please see below...", 50);

    private CommonMessages() {
    }
}
