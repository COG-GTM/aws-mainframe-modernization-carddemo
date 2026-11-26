package com.carddemo.programs.cbact02c;

import com.carddemo.programs.cbact02c.copybook.CardRecord;

final class TestRecords {

    private TestRecords() {
    }

    static String card(String cardNum, long acctId, int cvv, String name, String expiry, String status) {
        return new CardRecord(cardNum, acctId, cvv, name, expiry, status, "").toFixedWidth();
    }
}
