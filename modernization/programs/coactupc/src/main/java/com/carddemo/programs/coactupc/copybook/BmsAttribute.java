package com.carddemo.programs.coactupc.copybook;

/** DFHBMSCA - 3270 field attribute constants (EBCDIC byte value shown for reference). */
public enum BmsAttribute {
    /** Unprotected. */
    DFHBMUNP(0x40),
    /** Unprotected, numeric. */
    DFHBMUNN(0x50),
    /** Protected. */
    DFHBMPRO(0x60),
    /** Autoskip. */
    DFHBMASK(0xF0),
    /** Unprotected, bright. */
    DFHBMBRY(0xC8),
    /** Unprotected, dark. */
    DFHBMDAR(0x4C),
    /** Unprotected, MDT on (FSET). */
    DFHBMFSE(0xC1),
    /** Protected, MDT on (FSET). */
    DFHBMPRF(0x61),
    /** Autoskip, MDT on. */
    DFHBMASF(0xF1),
    /** Autoskip, bright. */
    DFHBMASB(0xF8);

    public final int ebcdic;

    BmsAttribute(int ebcdic) {
        this.ebcdic = ebcdic;
    }
}
