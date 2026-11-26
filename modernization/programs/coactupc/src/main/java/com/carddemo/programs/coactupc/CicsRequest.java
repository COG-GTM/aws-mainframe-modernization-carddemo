package com.carddemo.programs.coactupc;

import com.carddemo.programs.coactupc.copybook.AidKey;
import com.carddemo.programs.coactupc.copybook.CactupaInput;
import com.carddemo.programs.coactupc.copybook.CardDemoCommarea;

/**
 * One pseudo-conversational task: EIBAID, DFHCOMMAREA (CARDDEMO-COMMAREA followed by
 * WS-THIS-PROGCOMMAREA; a null commarea means EIBCALEN = 0) and the data RECEIVE MAP would return.
 *
 * @param eibaid             attention key pressed
 * @param commarea           CARDDEMO-COMMAREA part of DFHCOMMAREA, null when EIBCALEN = 0
 * @param programCommarea    WS-THIS-PROGCOMMAREA part; null when the caller passed only CARDDEMO-COMMAREA
 * @param map                CACTUPAI as received
 */
public record CicsRequest(AidKey eibaid, CardDemoCommarea commarea, AcctUpdateCommarea programCommarea,
        CactupaInput map) {

    /** First invocation of the transaction with no COMMAREA. */
    public static CicsRequest initial() {
        return new CicsRequest(AidKey.DFHENTER, null, null, CactupaInput.empty());
    }

    /** Transfer from another program (e.g. the main menu) with only CARDDEMO-COMMAREA. */
    public static CicsRequest fromProgram(CardDemoCommarea commarea) {
        return new CicsRequest(AidKey.DFHENTER, commarea, null, CactupaInput.empty());
    }
}
