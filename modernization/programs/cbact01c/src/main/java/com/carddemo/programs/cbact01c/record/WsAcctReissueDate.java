package com.carddemo.programs.cbact01c.record;

import com.carddemo.programs.cbact01c.support.Pic;

/**
 * WORKING-STORAGE {@code 01 WS-ACCT-REISSUE-DATE} (YYYY, filler, MM, filler, DD) and its
 * {@code WS-REISSUE-DATE REDEFINES ... PIC X(10)} view.
 */
public final class WsAcctReissueDate {
    public static final int LENGTH = 10;
    private String wsReissueDate = Pic.spaces(LENGTH);

    public void setWsReissueDate(String v) {
        wsReissueDate = Pic.x(v, LENGTH);
    }

    public String wsReissueDate() {
        return wsReissueDate;
    }

    public String wsAcctReissueYyyy() {
        return wsReissueDate.substring(0, 4);
    }

    public String wsFiller1() {
        return wsReissueDate.substring(4, 5);
    }

    public String wsAcctReissueMm() {
        return wsReissueDate.substring(5, 7);
    }

    public String wsFiller2() {
        return wsReissueDate.substring(7, 8);
    }

    public String wsAcctReissueDd() {
        return wsReissueDate.substring(8, 10);
    }
}
