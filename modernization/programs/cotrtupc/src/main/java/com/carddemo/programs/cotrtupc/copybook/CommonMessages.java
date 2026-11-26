package com.carddemo.programs.cotrtupc.copybook;

import com.carddemo.programs.cotrtupc.support.CobolText;

/** CSMSG01Y - CCDA-COMMON-MESSAGES (PIC X(50) each; the 49-char source literals are space-padded). */
public final class CommonMessages {

    public static final String CCDA_MSG_THANK_YOU =
            CobolText.pic("Thank you for using CardDemo application...      ", 50);
    public static final String CCDA_MSG_INVALID_KEY =
            CobolText.pic("Invalid key pressed. Please see below...         ", 50);

    private CommonMessages() {
    }
}
