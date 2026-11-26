package com.carddemo.programs.cousr01c.dto;

import com.carddemo.programs.cousr01c.copybook.CardDemoCommarea;
import com.carddemo.programs.cousr01c.copybook.Dfhaid;

/**
 * One CICS task for transaction CU01.
 *
 * @param commarea DFHCOMMAREA, or null when EIBCALEN = 0
 * @param aid      EIBAID
 * @param input    data received by RECEIVE MAP (only read on a re-entry)
 */
public record CicsRequest(CardDemoCommarea commarea, Dfhaid aid, UserAddScreenInput input) {

    public static CicsRequest noCommarea() {
        return new CicsRequest(null, Dfhaid.DFHENTER, UserAddScreenInput.empty());
    }
}
