package com.carddemo.programs.coadm01c.copybook;

/** COTTL01Y - 01 CCDA-SCREEN-TITLE. */
public final class ScreenTitle {

    /** CCDA-TITLE01 PIC X(40). */
    public static final String CCDA_TITLE01 = CobolFields.alnum("      AWS Mainframe Modernization       ", 40);
    /** CCDA-TITLE02 PIC X(40). */
    public static final String CCDA_TITLE02 = CobolFields.alnum("              CardDemo                  ", 40);
    /** CCDA-THANK-YOU PIC X(40). */
    public static final String CCDA_THANK_YOU = CobolFields.alnum("Thank you for using CCDA application... ", 40);

    private ScreenTitle() {
    }
}
