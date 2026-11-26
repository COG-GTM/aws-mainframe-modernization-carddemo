package com.carddemo.programs.cotrn01c;

import com.carddemo.programs.cotrn01c.copybook.Dfhaid;

/**
 * One pseudo-conversational task invocation of transaction CT01.
 *
 * @param dfhcommarea DFHCOMMAREA contents; null or empty means EIBCALEN = 0
 * @param eibaid      attention key pressed (EIBAID)
 * @param input       what RECEIVE MAP would return (used only on re-entry)
 */
public record CicsRequest(String dfhcommarea, Dfhaid eibaid, TrnViewRequest input) {

    public CicsRequest {
        if (input == null) {
            input = TrnViewRequest.empty();
        }
        if (eibaid == null) {
            eibaid = Dfhaid.DFHENTER;
        }
    }

    public int eibcalen() {
        return dfhcommarea == null ? 0 : dfhcommarea.length();
    }
}
