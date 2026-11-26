package com.carddemo.programs.comen01c;

import com.carddemo.programs.comen01c.copybook.Dfhaid;

/**
 * EIB fields and DFHCOMMAREA presented to COMEN01C. {@code commarea == null} (or empty) means
 * EIBCALEN = 0.
 */
public record CicsRequest(String commarea, Dfhaid eibaid) {

    public int eibcalen() {
        return commarea == null ? 0 : commarea.length();
    }
}
