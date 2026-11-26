package com.carddemo.programs.cotrn00c;

import com.carddemo.programs.cotrn00c.copybook.DfhAid;

/**
 * One CICS task of transaction CT00. {@code commarea == null} means EIBCALEN = 0.
 * {@code screen} is what RECEIVE MAP would return (only read on re-entry).
 */
public record Cotrn00cRequest(DfhAid eibaid, Cotrn00cCommarea commarea, ScreenInput screen) {

    public Cotrn00cRequest {
        if (eibaid == null) {
            throw new IllegalArgumentException("eibaid is required");
        }
        if (screen == null) {
            screen = ScreenInput.empty();
        }
    }
}
