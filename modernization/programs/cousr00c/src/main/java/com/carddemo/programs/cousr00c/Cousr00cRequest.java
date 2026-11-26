package com.carddemo.programs.cousr00c;

import com.carddemo.programs.cousr00c.copybook.Dfhaid;

/**
 * One COUSR00C task invocation.
 *
 * @param eibaid   EIBAID - attention key that started the task
 * @param commarea DFHCOMMAREA as fixed-width text; null or empty means EIBCALEN = 0
 * @param input    map fields delivered by RECEIVE MAP (used only on re-entry)
 */
public record Cousr00cRequest(Dfhaid eibaid, String commarea, Cousr0aInput input) {

    public Cousr00cRequest {
        input = input == null ? Cousr0aInput.empty() : input;
    }

    public int eibcalen() {
        return commarea == null ? 0 : commarea.length();
    }
}
