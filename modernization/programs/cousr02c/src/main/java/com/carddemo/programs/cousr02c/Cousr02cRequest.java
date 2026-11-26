package com.carddemo.programs.cousr02c;

import com.carddemo.programs.cousr02c.copybook.CardDemoCommarea;
import com.carddemo.programs.cousr02c.copybook.Dfhaid;

/**
 * One pseudo-conversational task of transaction CU02.
 *
 * @param dfhcommarea DFHCOMMAREA bytes; null or empty means EIBCALEN = 0
 * @param eibaid      attention key (EIBAID); only consulted on re-entry
 * @param mapInput    screen data returned by RECEIVE MAP; only consulted on re-entry
 */
public record Cousr02cRequest(String dfhcommarea, Dfhaid eibaid, MapInput mapInput) {

    public int eibcalen() {
        return dfhcommarea == null ? 0 : dfhcommarea.length();
    }

    public static Cousr02cRequest noCommarea() {
        return new Cousr02cRequest(null, Dfhaid.DFHENTER, MapInput.empty());
    }

    public static Cousr02cRequest firstEntry(CardDemoCommarea commarea) {
        return new Cousr02cRequest(commarea.toBytes(), Dfhaid.DFHENTER, MapInput.empty());
    }

    public static Cousr02cRequest reenter(CardDemoCommarea commarea, Dfhaid aid, MapInput input) {
        return new Cousr02cRequest(commarea.toBytes(), aid, input);
    }
}
