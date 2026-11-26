package com.carddemo.programs.coadm01c;

import com.carddemo.programs.coadm01c.copybook.CardDemoCommarea;
import com.carddemo.programs.coadm01c.copybook.Coadm1aInput;
import com.carddemo.programs.coadm01c.copybook.Dfhaid;

/**
 * One pseudo-conversational task of transaction CA00.
 *
 * @param dfhCommarea  DFHCOMMAREA bytes; {@code null}/empty means EIBCALEN = 0
 * @param eibAid       EIBAID
 * @param receivedMap  what RECEIVE MAP would map into COADM1AI; {@code null} = MAPFAIL (no data, map unchanged)
 */
public record Coadm01cRequest(String dfhCommarea, Dfhaid eibAid, Coadm1aInput receivedMap) {

    public int eibcalen() {
        return dfhCommarea == null ? 0 : dfhCommarea.length();
    }

    public static Coadm01cRequest withoutCommarea() {
        return new Coadm01cRequest(null, Dfhaid.DFHENTER, null);
    }

    public static Coadm01cRequest of(CardDemoCommarea commarea, Dfhaid aid, Coadm1aInput map) {
        return new Coadm01cRequest(commarea.toRecord(), aid, map);
    }
}
