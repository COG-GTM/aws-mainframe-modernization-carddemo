package com.carddemo.programs.cousr03c.copybook;

/**
 * DFHBMSCA (IBM-supplied) BMS attribute constants used by CardDemo. Colours are stored in the
 * ISO-8859-1 symbolic map as the character equivalent of their EBCDIC value (X'F1'..X'F7' = '1'..'7').
 */
public final class Dfhbmsca {
    public static final byte DFHDFCOL = 0x00;
    public static final byte DFHBLUE = '1';
    public static final byte DFHRED = '2';
    public static final byte DFHPINK = '3';
    public static final byte DFHGREEN = '4';
    public static final byte DFHTURQ = '5';
    public static final byte DFHYELLO = '6';
    public static final byte DFHNEUTR = '7';

    public static final byte DFHDFHI = 0x00;
    public static final byte DFHBLINK = '1';
    public static final byte DFHREVRS = '2';
    public static final byte DFHUNDLN = '4';

    /** Colour value name for a C-suffix byte; DFHDFCOL (X'00') means "use the BMS map default". */
    public static String colorName(byte b) {
        return switch (b) {
            case DFHDFCOL -> "DFHDFCOL";
            case DFHBLUE -> "DFHBLUE";
            case DFHRED -> "DFHRED";
            case DFHPINK -> "DFHPINK";
            case DFHGREEN -> "DFHGREEN";
            case DFHTURQ -> "DFHTURQ";
            case DFHYELLO -> "DFHYELLO";
            case DFHNEUTR -> "DFHNEUTR";
            default -> String.format("X'%02X'", b);
        };
    }

    private Dfhbmsca() {
    }
}
