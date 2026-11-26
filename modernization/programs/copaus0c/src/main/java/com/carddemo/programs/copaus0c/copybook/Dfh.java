package com.carddemo.programs.copaus0c.copybook;

/** DFHAID / DFHBMSCA subset used by COPAUS0C. */
public final class Dfh {
    /** DFHBMSCA DFHBMPRO - protected field attribute. */
    public static final char DFHBMPRO = '-';
    /** DFHBMSCA DFHBMUNP - unprotected field attribute. */
    public static final char DFHBMUNP = ' ';

    /** DFHAID attention identifiers. */
    public enum Aid {
        ENTER, CLEAR, PA1, PA2, PA3,
        PF1, PF2, PF3, PF4, PF5, PF6, PF7, PF8, PF9, PF10, PF11, PF12,
        PF13, PF14, PF15, PF16, PF17, PF18, PF19, PF20, PF21, PF22, PF23, PF24
    }

    private Dfh() {
    }
}
