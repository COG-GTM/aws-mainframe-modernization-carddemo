package com.carddemo.programs.coactupc.copybook;

/**
 * CSSETATY - COPY ... REPLACING (TESTVAR1)/(SCRNVAR2)/(MAPNAME3): highlight an input field in red
 * when its edit flag is NOT-OK or BLANK on a re-entry, and show '*' in it when it was blank.
 */
public final class ScreenAttributeSetter {

    private ScreenAttributeSetter() {
    }

    public static void apply(boolean flgNotOk, boolean flgBlank, boolean pgmReenter, CactupaOutput map,
            CactupaField field) {
        if ((flgNotOk || flgBlank) && pgmReenter) {
            map.setColor(field, BmsColor.DFHRED);
            if (flgBlank) {
                map.setData(field, "*");
            }
        }
    }
}
