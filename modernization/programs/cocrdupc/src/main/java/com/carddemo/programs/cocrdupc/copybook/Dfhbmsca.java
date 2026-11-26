package com.carddemo.programs.cocrdupc.copybook;

/**
 * DFHBMSCA (IBM-supplied BMS attribute constants) subset used by COCRDUPC. Values are the EBCDIC
 * byte codes from the IBM copybook.
 */
public final class Dfhbmsca {
    /** Unprotected, MDT on (FSET). */
    public static final int DFHBMFSE = 0xC1;
    /** Protected, MDT on (FSET). */
    public static final int DFHBMPRF = 0x61;
    /** Protected. */
    public static final int DFHBMPRO = 0x60;
    /** Unprotected. */
    public static final int DFHBMUNP = 0x40;
    /** Autoskip. */
    public static final int DFHBMASK = 0xF0;
    /** Dark (non-display). */
    public static final int DFHBMDAR = 0x4C;
    /** Bright. */
    public static final int DFHBMBRY = 0xC8;
    /** Default colour. */
    public static final int DFHDFCOL = 0x00;
    public static final int DFHBLUE = 0xF1;
    public static final int DFHRED = 0xF2;
    public static final int DFHPINK = 0xF3;
    public static final int DFHGREEN = 0xF4;
    public static final int DFHTURQ = 0xF5;
    public static final int DFHYELLO = 0xF6;
    public static final int DFHNEUTR = 0xF7;

    private Dfhbmsca() {
    }
}
