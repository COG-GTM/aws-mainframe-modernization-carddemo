package com.carddemo.online.cocrdlic;

/** The DFHBMSCA (IBM BMS attribute) constants COCRDLIC uses, as their EBCDIC byte values. */
final class Dfhbmsca {

    /** DFHBMPRO '-': protected. */
    static final int DFHBMPRO = 0x60;
    /** DFHBMPRF '/': protected, MDT on. */
    static final int DFHBMPRF = 0x61;
    /** DFHBMFSE 'A': unprotected, MDT on. */
    static final int DFHBMFSE = 0xC1;
    /** DFHBMDAR '&lt;': dark (an attribute constant; COCRDLIC moves it into a colour byte). */
    static final int DFHBMDAR = 0x4C;
    /** DFHRED '2'. */
    static final int DFHRED = 0xF2;
    /** DFHNEUTR '7'. */
    static final int DFHNEUTR = 0xF7;

    private Dfhbmsca() {
    }
}
