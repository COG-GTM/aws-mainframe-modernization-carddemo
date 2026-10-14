package com.carddemo.runtime.bms;

/**
 * DFHBMSCA constants (attribute and colour bytes, EBCDIC values) for {@link SymbolicMap#setAttribute} /
 * {@link SymbolicMap#setColor}.
 */
public final class BmsAttributes {

    /** Unprotected, normal (' '). */
    public static final int DFHBMUNP = 0x40;
    /** Unprotected, numeric ('&amp;'). */
    public static final int DFHBMUNN = 0x50;
    /** Protected ('-'). */
    public static final int DFHBMPRO = 0x60;
    /** Protected + MDT ('/'). */
    public static final int DFHBMPRF = 0x61;
    /** Unprotected + MDT ('A'). */
    public static final int DFHBMFSE = 0xC1;
    /** Unprotected, bright ('H'). */
    public static final int DFHBMBRY = 0xC8;
    /** Unprotected, dark ('&lt;'). */
    public static final int DFHBMDAR = 0x4C;
    /** Autoskip ('0'). */
    public static final int DFHBMASK = 0xF0;
    /** Autoskip + MDT ('1'). */
    public static final int DFHBMASF = 0xF1;
    /** Autoskip, bright ('8'). */
    public static final int DFHBMASB = 0xF8;

    public static final int DFHDFCOL = 0x00;
    public static final int DFHBLUE = 0xF1;
    public static final int DFHRED = 0xF2;
    public static final int DFHPINK = 0xF3;
    public static final int DFHGREEN = 0xF4;
    public static final int DFHTURQ = 0xF5;
    public static final int DFHYELLO = 0xF6;
    public static final int DFHNEUTR = 0xF7;

    private BmsAttributes() {
    }
}
