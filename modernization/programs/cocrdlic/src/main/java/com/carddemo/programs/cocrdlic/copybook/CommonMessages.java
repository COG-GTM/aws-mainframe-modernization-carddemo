package com.carddemo.programs.cocrdlic.copybook;

/** CSMSG01Y - CCDA-COMMON-MESSAGES (each PIC X(50)). */
public final class CommonMessages {
    public static final String CCDA_MSG_THANK_YOU = Pic.x("Thank you for using CardDemo application...", 50);
    public static final String CCDA_MSG_INVALID_KEY = Pic.x("Invalid key pressed. Please see below...", 50);

    private CommonMessages() {
    }
}
