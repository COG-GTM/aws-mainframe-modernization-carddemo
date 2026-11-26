package com.carddemo.programs.cocrdlic;

import com.carddemo.programs.cocrdlic.copybook.CardListMap;

import java.util.Arrays;

/**
 * Request DTO: the CCRDLIAI fields COCRDLIC reads in 2100-RECEIVE-SCREEN.
 * {@code null} means the field was not transmitted (LOW-VALUES); otherwise BMS pads input with spaces.
 */
public record CardListScreenInput(String acctFilter, String cardFilter, String[] selections) {

    public CardListScreenInput {
        String[] s = new String[CardListMap.ROWS];
        if (selections != null) {
            System.arraycopy(selections, 0, s, 0, Math.min(selections.length, s.length));
        }
        selections = s;
    }

    public static CardListScreenInput empty() {
        return new CardListScreenInput(null, null, null);
    }

    public static CardListScreenInput filters(String acctFilter, String cardFilter) {
        return new CardListScreenInput(acctFilter, cardFilter, null);
    }

    /** Builder-style copy with one selection code (row is 1-based). */
    public CardListScreenInput withSelection(int row, String code) {
        String[] s = Arrays.copyOf(selections, selections.length);
        s[row - 1] = code;
        return new CardListScreenInput(acctFilter, cardFilter, s);
    }

    public String selection(int row) {
        return selections[row - 1];
    }
}
