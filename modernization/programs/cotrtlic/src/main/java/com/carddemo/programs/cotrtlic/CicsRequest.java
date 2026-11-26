package com.carddemo.programs.cotrtlic;

import com.carddemo.programs.cotrtlic.copybook.CotrtliMap;
import com.carddemo.programs.cotrtlic.copybook.Dfhaid;

/**
 * One CICS task for transaction CTLI.
 *
 * @param eibaid      EIBAID
 * @param commarea    DFHCOMMAREA; {@code null} means EIBCALEN = 0
 * @param terminalMap data returned by RECEIVE MAP (only read when the program receives); may be null
 */
public record CicsRequest(Dfhaid eibaid, ProgramCommarea commarea, CotrtliMap terminalMap) {

    public static CicsRequest firstEntry(Dfhaid aid) {
        return new CicsRequest(aid, null, null);
    }
}
