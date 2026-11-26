package com.carddemo.programs.cotrtlic.copybook;

/**
 * IBM copybook DFHBMSCA: BMS attribute and colour constants used by COTRTLIC. Values are the
 * EBCDIC-printable characters IBM defines for each constant.
 */
public final class Dfhbmsca {
    /** Protected, MDT on (X'61'). */
    public static final char DFHBMPRF = '/';
    /** Protected (X'60'). */
    public static final char DFHBMPRO = '-';
    /** Unprotected, MDT on (X'C1'). */
    public static final char DFHBMFSE = 'A';
    /** Autoskip, MDT on (X'F1'). */
    public static final char DFHBMASF = '1';
    /** Dark / non-display (X'4C'). */
    public static final char DFHBMDAR = '<';
    public static final char DFHBLUE = '1';
    public static final char DFHRED = '2';
    public static final char DFHPINK = '3';
    public static final char DFHGREEN = '4';
    public static final char DFHTURQ = '5';
    public static final char DFHYELLO = '6';
    public static final char DFHNEUTR = '7';

    private Dfhbmsca() {
    }
}
