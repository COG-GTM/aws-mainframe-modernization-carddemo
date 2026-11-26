package com.carddemo.programs.cotrtupc.copybook;

/** IBM DFHBMSCA - BMS attribute / colour constants (EBCDIC byte values), the subset used by COTRTUPC and peers. */
public final class Dfhbmsca {

    public static final char DFHBMUNP = (char) 0x40;  // unprotected
    public static final char DFHBMUNN = (char) 0x50;  // unprotected numeric
    public static final char DFHBMPRO = (char) 0x60;  // protected
    public static final char DFHBMPRF = (char) 0x61;  // protected, MDT on
    public static final char DFHBMASK = (char) 0xF0;  // autoskip
    public static final char DFHBMASF = (char) 0xF1;  // autoskip, MDT on
    public static final char DFHBMASB = (char) 0xF8;  // autoskip, bright
    public static final char DFHBMBRY = (char) 0xC8;  // bright
    public static final char DFHBMDAR = (char) 0x4C;  // dark
    public static final char DFHBMFSE = (char) 0xC1;  // unprotected, MDT on

    public static final char DFHDFCOL = (char) 0x00;
    public static final char DFHBLUE = (char) 0xF1;
    public static final char DFHRED = (char) 0xF2;
    public static final char DFHPINK = (char) 0xF3;
    public static final char DFHGREEN = (char) 0xF4;
    public static final char DFHTURQ = (char) 0xF5;
    public static final char DFHYELLO = (char) 0xF6;
    public static final char DFHNEUTR = (char) 0xF7;

    private Dfhbmsca() {
    }
}
