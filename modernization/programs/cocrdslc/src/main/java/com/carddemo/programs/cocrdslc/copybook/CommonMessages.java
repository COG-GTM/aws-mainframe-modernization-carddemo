package com.carddemo.programs.cocrdslc.copybook;

import com.carddemo.programs.cocrdslc.CobolText;

/** CSMSG01Y - CCDA-COMMON-MESSAGES (each PIC X(50)). */
public final class CommonMessages {
    public static final String CCDA_MSG_THANK_YOU = CobolText.pic("Thank you for using CardDemo application...      ", 50);
    public static final String CCDA_MSG_INVALID_KEY = CobolText.pic("Invalid key pressed. Please see below...         ", 50);

    private CommonMessages() {
    }
}
