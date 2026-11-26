package com.carddemo.programs.cobil00c;

import com.carddemo.programs.cobil00c.copybook.CardDemoCommarea;
import com.carddemo.programs.cobil00c.copybook.Dfhaid;

/**
 * One pseudo-conversational task of transaction CB00: EIBCALEN, DFHCOMMAREA, EIBAID and the
 * data the terminal returns for RECEIVE MAP.
 */
public record BillPayRequest(int eibcalen, String dfhcommarea, Dfhaid eibaid, BillPayScreenInput screen) {

    public BillPayRequest {
        dfhcommarea = dfhcommarea == null ? "" : dfhcommarea;
        screen = screen == null ? BillPayScreenInput.empty() : screen;
    }

    /** Task started with no COMMAREA (EIBCALEN = 0). */
    public static BillPayRequest noCommarea() {
        return new BillPayRequest(0, "", Dfhaid.DFHENTER, null);
    }

    public static BillPayRequest of(CardDemoCommarea commarea, Dfhaid aid, BillPayScreenInput screen) {
        String bytes = commarea.toBytes();
        return new BillPayRequest(bytes.length(), bytes, aid, screen);
    }
}
