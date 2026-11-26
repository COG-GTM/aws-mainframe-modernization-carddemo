package com.carddemo.programs.coactupc.copybook;

/** DFHBMSCA - extended colour attribute constants. */
public enum BmsColor {
    DFHDFCOL(0x00), DFHBLUE(0xF1), DFHRED(0xF2), DFHPINK(0xF3), DFHGREEN(0xF4), DFHTURQ(0xF5),
    DFHYELLO(0xF6), DFHNEUTR(0xF7);

    public final int ebcdic;

    BmsColor(int ebcdic) {
        this.ebcdic = ebcdic;
    }
}
