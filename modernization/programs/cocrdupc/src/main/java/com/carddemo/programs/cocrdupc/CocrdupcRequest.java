package com.carddemo.programs.cocrdupc;

import com.carddemo.programs.cocrdupc.copybook.AidKey;

/**
 * One CICS task for transaction CCUP: DFHCOMMAREA (null or empty means EIBCALEN = 0), EIBAID and
 * the map data the terminal sent.
 */
public record CocrdupcRequest(String commarea, AidKey eibaid, CcrdupaInput mapInput) {

    public int eibcalen() {
        return commarea == null ? 0 : commarea.length();
    }
}
