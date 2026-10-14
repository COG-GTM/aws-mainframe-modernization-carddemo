package com.carddemo.common.copybook;

import com.carddemo.runtime.cobol.PicX;

/** CSMSG01Y {@code CCDA-COMMON-MESSAGES} constants (each PIC X(50), values copied verbatim). */
public final class CommonMessages {

    public static final String CCDA_MSG_THANK_YOU = PicX.of("Thank you for using CardDemo application...      ", 50);
    public static final String CCDA_MSG_INVALID_KEY = PicX.of("Invalid key pressed. Please see below...         ", 50);

    private CommonMessages() {
    }
}
