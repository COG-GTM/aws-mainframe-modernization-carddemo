package com.carddemo.common.copybook;

import com.carddemo.runtime.cobol.PicX;

/** COTTL01Y {@code CCDA-SCREEN-TITLE} constants (each PIC X(40), values copied verbatim). */
public final class ScreenTitles {

    public static final String CCDA_TITLE01 = PicX.of("      AWS Mainframe Modernization       ", 40);
    public static final String CCDA_TITLE02 = PicX.of("              CardDemo                  ", 40);
    public static final String CCDA_THANK_YOU = PicX.of("Thank you for using CCDA application... ", 40);

    private ScreenTitles() {
    }
}
