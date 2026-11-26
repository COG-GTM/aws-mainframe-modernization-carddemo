package com.carddemo.programs.coactvwc;

import com.carddemo.programs.coactvwc.copybook.CactvwaInput;
import com.carddemo.programs.coactvwc.copybook.DfhAid;

/**
 * One CICS task for transaction CAVW.
 *
 * @param eibaid      EIBAID (key pressed)
 * @param dfhcommarea DFHCOMMAREA contents; {@code null} means EIBCALEN = 0
 * @param mapInput    what RECEIVE MAP would return for CACTVWA (only used on re-entry); may be {@code null}
 */
public record CoactvwcRequest(DfhAid eibaid, String dfhcommarea, CactvwaInput mapInput) {

    public int eibcalen() {
        return dfhcommarea == null ? 0 : dfhcommarea.length();
    }
}
