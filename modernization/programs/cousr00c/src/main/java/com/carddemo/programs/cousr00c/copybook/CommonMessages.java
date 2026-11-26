package com.carddemo.programs.cousr00c.copybook;

import com.carddemo.programs.cousr00c.Cobol;

/** CSMSG01Y - CCDA-COMMON-MESSAGES (each PIC X(50)). */
public final class CommonMessages {

    public static final String CCDA_MSG_THANK_YOU = Cobol.alnum("Thank you for using CardDemo application...      ", 50);
    public static final String CCDA_MSG_INVALID_KEY = Cobol.alnum("Invalid key pressed. Please see below...         ", 50);

    private CommonMessages() {
    }
}
