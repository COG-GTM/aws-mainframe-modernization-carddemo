package com.carddemo.programs.cotrtlic.copybook;

/** EXEC SQL INCLUDE SQLCA: the subset of the SQL communication area consumed by COTRTLIC. */
public final class Sqlca {
    /** SQLCODE PIC S9(9) COMP-5. */
    public int sqlcode;
    /** SQLERRMC PIC X(70) (message tokens). */
    public String sqlerrmc = "";
    /** SQLSTATE PIC X(5). */
    public String sqlstate = "00000";

    public void set(int sqlcode, String sqlerrmc, String sqlstate) {
        this.sqlcode = sqlcode;
        this.sqlerrmc = sqlerrmc == null ? "" : sqlerrmc;
        this.sqlstate = sqlstate == null ? "00000" : sqlstate;
    }
}
