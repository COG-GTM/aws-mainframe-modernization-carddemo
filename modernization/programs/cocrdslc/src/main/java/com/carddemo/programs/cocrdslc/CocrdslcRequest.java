package com.carddemo.programs.cocrdslc;

import com.carddemo.programs.cocrdslc.copybook.CarddemoCommarea;
import com.carddemo.programs.cocrdslc.copybook.CcrdslaInput;
import com.carddemo.programs.cocrdslc.copybook.Dfhaid;

/**
 * One CICS task invocation of COCRDSLC.
 *
 * @param commarea DFHCOMMAREA as fixed-width text; {@code null} or empty means EIBCALEN = 0
 * @param eibaid   attention key (EIBAID)
 * @param map      data RECEIVEd from map CCRDSLA (only read on re-entry); may be {@code null}
 */
public record CocrdslcRequest(String commarea, Dfhaid eibaid, CcrdslaInput map) {

    public int eibcalen() {
        return commarea == null ? 0 : commarea.length();
    }

    public CcrdslaInput mapOrEmpty() {
        return map == null ? CcrdslaInput.of(null, null) : map;
    }

    public static CocrdslcRequest firstTime(Dfhaid aid) {
        return new CocrdslcRequest(null, aid, null);
    }

    public static CocrdslcRequest of(CarddemoCommarea commarea, Dfhaid aid, CcrdslaInput map) {
        return new CocrdslcRequest(commarea.toFixedWidth(), aid, map);
    }

    public static CocrdslcRequest of(CarddemoCommarea commarea, ThisProgCommarea thisProg, Dfhaid aid,
                                     CcrdslaInput map) {
        return new CocrdslcRequest(commarea.toFixedWidth() + thisProg.toFixedWidth(), aid, map);
    }
}
