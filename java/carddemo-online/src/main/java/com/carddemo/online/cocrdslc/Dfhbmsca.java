package com.carddemo.online.cocrdslc;

/**
 * The DFHBMSCA attribute/colour constants used by COCRDSLC, as their EBCDIC byte values (the value the COBOL
 * program MOVEs into the symbolic map's {@code nameA}/{@code nameC} bytes).
 */
final class Dfhbmsca {

    /** DFHBMPRF: protected, MDT on ('/' = X'61'). */
    static final int DFHBMPRF = 0x61;
    /** DFHBMFSE: unprotected, MDT on ('A' = X'C1'). */
    static final int DFHBMFSE = 0xC1;
    /** DFHBMDAR: unprotected, dark ('<' = X'4C'); COCRDSLC moves it into a colour byte. */
    static final int DFHBMDAR = 0x4C;
    /** DFHDFCOL: default colour (X'00'). */
    static final int DFHDFCOL = 0x00;
    /** DFHRED (X'F2'). */
    static final int DFHRED = 0xF2;
    /** DFHNEUTR (X'F7'). */
    static final int DFHNEUTR = 0xF7;

    private Dfhbmsca() {
    }
}
