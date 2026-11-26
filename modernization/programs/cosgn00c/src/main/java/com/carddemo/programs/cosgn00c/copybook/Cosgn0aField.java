package com.carddemo.programs.cosgn00c.copybook;

/** Named fields of BMS map COSGN0A (mapset COSGN00) with their data lengths. */
public enum Cosgn0aField {
    TRNNAME(4),
    TITLE01(40),
    CURDATE(8),
    PGMNAME(8),
    TITLE02(40),
    CURTIME(9),
    APPLID(8),
    SYSID(8),
    USERID(8),
    PASSWD(8),
    ERRMSG(78);

    private final int length;

    Cosgn0aField(int length) {
        this.length = length;
    }

    public int length() {
        return length;
    }
}
