package com.carddemo.programs.cocrdlic;

import com.carddemo.programs.cocrdlic.copybook.CardDemoCommarea;
import com.carddemo.programs.cocrdlic.copybook.EibAid;

/**
 * One CICS task invocation of transaction CCLI.
 *
 * @param commarea DFHCOMMAREA image (CARDDEMO-COMMAREA + WS-THIS-PROGCOMMAREA); null/empty means EIBCALEN = 0
 * @param eibaid   attention key pressed
 * @param screen   map input, used only when RECEIVE MAP is performed
 */
public record CardListRequest(String commarea, EibAid eibaid, CardListScreenInput screen) {

    public CardListRequest {
        if (eibaid == null) {
            eibaid = EibAid.ENTER;
        }
        if (screen == null) {
            screen = CardListScreenInput.empty();
        }
    }

    public int eibcalen() {
        return commarea == null ? 0 : commarea.length();
    }

    /** First entry (EIBCALEN = 0). */
    public static CardListRequest initial() {
        return new CardListRequest(null, EibAid.ENTER, null);
    }

    /** Called by another program (e.g. the menu via XCTL) with only CARDDEMO-COMMAREA. */
    public static CardListRequest fromProgram(CardDemoCommarea commarea, EibAid aid) {
        return new CardListRequest(commarea.toFixedWidth(), aid, null);
    }

    /** Pseudo-conversational re-entry with the COMMAREA returned by the previous task. */
    public static CardListRequest reenter(String previousCommarea, EibAid aid, CardListScreenInput screen) {
        return new CardListRequest(previousCommarea, aid, screen);
    }
}
