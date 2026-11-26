package com.carddemo.programs.copaus1c.copybook;

/** DFHBMSCA - extended colour attribute constants (DFHxxxx PIC X VALUE ...). */
public enum BmsColor {
    DFHDFCOL('\u0000'), DFHBLUE('1'), DFHRED('2'), DFHPINK('3'), DFHGREEN('4'),
    DFHTURQ('5'), DFHYELLO('6'), DFHNEUTR('7');

    public final char code;

    BmsColor(char code) {
        this.code = code;
    }
}
