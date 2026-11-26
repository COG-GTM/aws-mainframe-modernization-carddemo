package com.carddemo.programs.cobil00c;

import com.carddemo.programs.cobil00c.copybook.Cobol;

/**
 * One {@code EXEC CICS SEND MAP('COBIL0A') MAPSET('COBIL00') FROM(COBIL0AO) ERASE CURSOR}.
 * Field values are the raw fixed-width COBIL0AO data: X'00' bytes are low-values (not
 * transmitted, so the terminal shows the field blank). {@code errmsgColor} is ERRMSGC
 * (X'00' = map default RED, '4' = DFHGREEN).
 */
public record BillPayScreen(
        String trnname,
        String title01,
        String curdate,
        String pgmname,
        String title02,
        String curtime,
        String actidin,
        String curbal,
        String confirm,
        String errmsg,
        char errmsgColor,
        String cursorField) {

    /** Field text as the operator would see it: low-values shown as blanks, trailing blanks removed. */
    public static String visible(String raw) {
        return raw.replace(Cobol.LOW_VALUE, ' ').stripTrailing();
    }

    public String message() {
        return visible(errmsg);
    }
}
