package com.carddemo.programs.cotrtupc;

import com.carddemo.programs.cotrtupc.copybook.Dfhaid;

/**
 * One pseudo-conversational task of transaction CTTU: EIBAID, the DFHCOMMAREA received (null/empty
 * = EIBCALEN 0) and the modified input fields of map CTRTUPA (null = not transmitted, i.e. LOW-VALUES).
 */
public record TranTypeMaintRequest(Dfhaid aid, String commarea, String trtypcdI, String trtydscI) {

    public static TranTypeMaintRequest firstEntry(Dfhaid aid) {
        return new TranTypeMaintRequest(aid, null, null, null);
    }

    public int eibcalen() {
        return commarea == null ? 0 : commarea.length();
    }
}
