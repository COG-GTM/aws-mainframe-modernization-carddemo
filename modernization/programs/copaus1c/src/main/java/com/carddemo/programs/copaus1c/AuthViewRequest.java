package com.carddemo.programs.copaus1c;

import com.carddemo.programs.copaus1c.copybook.AidKey;

/**
 * One CICS task of transaction CPVD: the DFHCOMMAREA passed in ({@code null} means EIBCALEN = 0) and the
 * attention key (EIBAID). COPAU1A has only protected (ASKIP) fields, so RECEIVE MAP carries no user input.
 */
public record AuthViewRequest(Copaus1cCommarea commarea, AidKey aid) {

    public static AuthViewRequest noCommarea() {
        return new AuthViewRequest(null, AidKey.DFHENTER);
    }
}
