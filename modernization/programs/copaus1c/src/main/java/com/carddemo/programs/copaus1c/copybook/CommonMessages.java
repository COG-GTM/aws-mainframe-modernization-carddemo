package com.carddemo.programs.copaus1c.copybook;

/** CSMSG01Y - CCDA-COMMON-MESSAGES (PIC X(50) each). */
public final class CommonMessages {
    public static final String CCDA_MSG_THANK_YOU = Cobol.x("Thank you for using CardDemo application...", 50);
    public static final String CCDA_MSG_INVALID_KEY = Cobol.x("Invalid key pressed. Please see below...", 50);

    private CommonMessages() {
    }
}
