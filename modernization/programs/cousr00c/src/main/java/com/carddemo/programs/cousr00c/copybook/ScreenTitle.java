package com.carddemo.programs.cousr00c.copybook;

import com.carddemo.programs.cousr00c.Cobol;

/** COTTL01Y - CCDA-SCREEN-TITLE (each PIC X(40)). */
public final class ScreenTitle {

    public static final String CCDA_TITLE01 = Cobol.alnum("      AWS Mainframe Modernization       ", 40);
    public static final String CCDA_TITLE02 = Cobol.alnum("              CardDemo                  ", 40);
    public static final String CCDA_THANK_YOU = Cobol.alnum("Thank you for using CCDA application... ", 40);

    private ScreenTitle() {
    }
}
