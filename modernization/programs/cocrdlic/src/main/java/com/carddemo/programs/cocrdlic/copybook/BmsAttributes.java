package com.carddemo.programs.cocrdlic.copybook;

/** DFHBMSCA - IBM-supplied BMS attribute and colour constants used by COCRDLIC (EBCDIC display chars). */
public final class BmsAttributes {
    public static final char DFHBMPRO = '-';   // protected
    public static final char DFHBMPRF = '/';   // protected + MDT (FSET)
    public static final char DFHBMFSE = 'A';   // unprotected + MDT (FSET)
    public static final char DFHBMASK = '0';   // autoskip
    public static final char DFHBMUNP = ' ';   // unprotected
    public static final char DFHBMBRY = 'H';   // bright
    public static final char DFHBMDAR = '<';   // dark
    public static final char DFHDFCOL = Pic.LOW_VALUE;
    public static final char DFHBLUE = '1';
    public static final char DFHRED = '2';
    public static final char DFHPINK = '3';
    public static final char DFHGREEN = '4';
    public static final char DFHTURQ = '5';
    public static final char DFHYELLO = '6';
    public static final char DFHNEUTR = '7';

    private BmsAttributes() {
    }
}
