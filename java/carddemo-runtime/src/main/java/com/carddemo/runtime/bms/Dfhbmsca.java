package com.carddemo.runtime.bms;

/**
 * IBM DFHBMSCA: standard 3270 attribute and colour bytes (EBCDIC values) for
 * {@link SymbolicMap#setAttribute} / {@link SymbolicMap#setColor}.
 */
public final class Dfhbmsca {

    /** Unprotected. */
    public static final int DFHBMUNP = 0x40;
    /** Unprotected, numeric. */
    public static final int DFHBMUNN = 0x50;
    /** Protected. */
    public static final int DFHBMPRO = 0x60;
    /** Protected, MDT on (FSET). */
    public static final int DFHBMPRF = 0x61;
    /** Unprotected, MDT on (FSET). */
    public static final int DFHBMFSE = 0xC1;
    /** Unprotected, bright. */
    public static final int DFHBMBRY = 0xC8;
    /** Unprotected, dark. */
    public static final int DFHBMDAR = 0x4C;
    /** Autoskip. */
    public static final int DFHBMASK = 0xF0;
    /** Autoskip, MDT on. */
    public static final int DFHBMASF = 0xF1;
    /** Autoskip, bright. */
    public static final int DFHBMASB = 0xF8;

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

    public static boolean isProtected(int attributeByte) {
        return (attributeByte & 0x20) != 0;
    }

    /** Modified data tag: the terminal transmits the field on the next AID key. */
    public static boolean isMdt(int attributeByte) {
        return (attributeByte & 0x01) != 0;
    }

    public static boolean isDark(int attributeByte) {
        return (attributeByte & 0x0C) == 0x0C;
    }
}
