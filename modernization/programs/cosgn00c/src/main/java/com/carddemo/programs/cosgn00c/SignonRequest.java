package com.carddemo.programs.cosgn00c;

import com.carddemo.programs.cosgn00c.copybook.CicsAid;
import com.carddemo.programs.cosgn00c.copybook.Cosgn0aInput;

/**
 * One pseudo-conversational task of transaction CC00.
 *
 * @param eibcalen length of the incoming DFHCOMMAREA (0 on first entry)
 * @param eibaid   attention key pressed (EIBAID)
 * @param map      data RECEIVEd from map COSGN0A (only read on ENTER)
 */
public record SignonRequest(int eibcalen, CicsAid eibaid, Cosgn0aInput map) {

    public static SignonRequest firstEntry() {
        return new SignonRequest(0, CicsAid.DFHNULL, Cosgn0aInput.of(null, null));
    }

    public static SignonRequest enter(String userId, String password) {
        return new SignonRequest(1, CicsAid.DFHENTER, Cosgn0aInput.of(userId, password));
    }

    public static SignonRequest key(CicsAid aid) {
        return new SignonRequest(1, aid, Cosgn0aInput.of(null, null));
    }
}
