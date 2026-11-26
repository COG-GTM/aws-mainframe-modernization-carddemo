package com.carddemo.programs.coadm01c.copybook;

/** DFHBMSCA (IBM CICS supplied) - BMS extended colour attribute values used by CardDemo screens. */
public enum Dfhbmsca {
    DFHDFCOL('\u0000'),
    DFHBLUE('1'),
    DFHRED('2'),
    DFHPINK('3'),
    DFHGREEN('4'),
    DFHTURQ('5'),
    DFHYELLO('6'),
    DFHNEUTR('7');

    public final char code;

    Dfhbmsca(char code) {
        this.code = code;
    }
}
