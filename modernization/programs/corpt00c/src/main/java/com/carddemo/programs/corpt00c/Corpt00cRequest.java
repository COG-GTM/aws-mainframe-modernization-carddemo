package com.carddemo.programs.corpt00c;

import com.carddemo.programs.corpt00c.copybook.CarddemoCommarea;
import com.carddemo.programs.corpt00c.copybook.Corpt0aInput;
import com.carddemo.programs.corpt00c.copybook.Dfhaid;

/**
 * One CICS task invocation of transaction CR00.
 *
 * @param commarea DFHCOMMAREA, or null when EIBCALEN = 0
 * @param aid      EIBAID
 * @param map      data that EXEC CICS RECEIVE MAP would return (used only on re-entry)
 */
public record Corpt00cRequest(CarddemoCommarea commarea, Dfhaid aid, Corpt0aInput map) {

    public static Corpt00cRequest noCommarea() {
        return new Corpt00cRequest(null, Dfhaid.DFHNULL, Corpt0aInput.empty());
    }
}
