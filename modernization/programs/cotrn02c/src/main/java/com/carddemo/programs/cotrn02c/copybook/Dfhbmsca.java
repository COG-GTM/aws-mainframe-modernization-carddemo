package com.carddemo.programs.cotrn02c.copybook;

/** DFHBMSCA colour attribute constants (the C byte of a symbolic-map field). */
public enum Dfhbmsca {
    /** LOW-VALUES: use the colour defined in the physical map. */
    DFHDFCOL(Pic.LOW_VALUE),
    DFHBLUE('1'), DFHRED('2'), DFHPINK('3'), DFHGREEN('4'), DFHTURQ('5'), DFHYELLO('6'), DFHNEUTR('7');

    public final char code;

    Dfhbmsca(char code) {
        this.code = code;
    }

    public static Dfhbmsca of(char code) {
        for (Dfhbmsca c : values()) {
            if (c.code == code) {
                return c;
            }
        }
        return DFHDFCOL;
    }
}
