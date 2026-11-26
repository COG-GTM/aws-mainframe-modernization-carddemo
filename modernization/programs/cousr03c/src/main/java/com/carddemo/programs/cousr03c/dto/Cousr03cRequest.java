package com.carddemo.programs.cousr03c.dto;

import com.carddemo.programs.cousr03c.copybook.Dfhaid;

/**
 * One pseudo-conversational task of transaction CU03.
 *
 * @param dfhcommarea DFHCOMMAREA bytes; null or empty means EIBCALEN = 0
 * @param eibaid      attention key (EIBAID)
 * @param mapInput    data received from COUSR3A; null means RECEIVE MAP got MAPFAIL
 */
public record Cousr03cRequest(byte[] dfhcommarea, Dfhaid eibaid, ScreenInput mapInput) {

    public int eibcalen() {
        return dfhcommarea == null ? 0 : dfhcommarea.length;
    }
}
