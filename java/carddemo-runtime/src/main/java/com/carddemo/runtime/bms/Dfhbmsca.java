package com.carddemo.runtime.bms;

import java.util.Map;

/**
 * DFHBMSCA extended-colour values (moved to the symbolic map's {@code xxxC} byte) and the BMS COLOR names
 * they stand for.
 */
public final class Dfhbmsca {

    public static final int DFHDFCOL = 0x00;
    public static final int DFHBLUE = 0xF1;
    public static final int DFHRED = 0xF2;
    public static final int DFHPINK = 0xF3;
    public static final int DFHGREEN = 0xF4;
    public static final int DFHTURQ = 0xF5;
    public static final int DFHYELLO = 0xF6;
    public static final int DFHNEUTR = 0xF7;

    private static final Map<Integer, String> COLOR_NAMES = Map.of(DFHBLUE, "BLUE", DFHRED, "RED", DFHPINK, "PINK",
            DFHGREEN, "GREEN", DFHTURQ, "TURQUOISE", DFHYELLO, "YELLOW", DFHNEUTR, "NEUTRAL");

    private Dfhbmsca() {
    }

    /** BMS COLOR name of a colour byte; null for DFHDFCOL / unknown values (the map's COLOR applies). */
    public static String colorName(int colorByte) {
        return COLOR_NAMES.get(colorByte & 0xFF);
    }
}
