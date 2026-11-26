package com.carddemo.programs.cobtupdt.copybook;

import java.util.Arrays;

/**
 * DB2 SQL communication area ({@code EXEC SQL INCLUDE SQLCA END-EXEC}).
 *
 * <pre>
 * 01 SQLCA.
 *    05 SQLCAID     PIC X(8).
 *    05 SQLCABC     PIC S9(9) COMP-5.
 *    05 SQLCODE     PIC S9(9) COMP-5.
 *    05 SQLERRM.
 *       49 SQLERRML PIC S9(4) COMP-5.
 *       49 SQLERRMC PIC X(70).
 *    05 SQLERRP     PIC X(8).
 *    05 SQLERRD     OCCURS 6 TIMES PIC S9(9) COMP-5.
 *    05 SQLWARN.
 *       10 SQLWARN0 .. SQLWARN7 PIC X.
 *    05 SQLEXT.
 *       10 SQLWARN8 SQLWARN9 SQLWARNA PIC X.
 *       10 SQLSTATE PIC X(5).
 * </pre>
 *
 * COBTUPDT only reads SQLCODE (0, +100, &lt; 0); the other fields are carried for completeness.
 */
public final class Sqlca {

    public static final int SQLCABC_LENGTH = 136;
    public static final int SQLCODE_OK = 0;
    public static final int SQLCODE_NOT_FOUND = 100;

    private final String sqlcaid = "SQLCA   ";
    private final int sqlcabc = SQLCABC_LENGTH;
    private int sqlcode;
    private String sqlerrmc = Pic.alphanumeric("", 70);
    private String sqlerrp = Pic.alphanumeric("", 8);
    private final int[] sqlerrd = new int[6];
    private final char[] sqlwarn = new char[11];
    private String sqlstate = "00000";

    public Sqlca() {
        Arrays.fill(sqlwarn, ' ');
    }

    public String sqlcaid() {
        return sqlcaid;
    }

    public int sqlcabc() {
        return sqlcabc;
    }

    public int sqlcode() {
        return sqlcode;
    }

    public void setSqlcode(int sqlcode) {
        this.sqlcode = sqlcode;
    }

    /** SQLERRML: number of significant bytes in SQLERRMC. */
    public short sqlerrml() {
        return (short) sqlerrmc.stripTrailing().length();
    }

    public String sqlerrmc() {
        return sqlerrmc;
    }

    public void setSqlerrmc(String text) {
        this.sqlerrmc = Pic.alphanumeric(text, 70);
    }

    public String sqlerrp() {
        return sqlerrp;
    }

    public void setSqlerrp(String sqlerrp) {
        this.sqlerrp = Pic.alphanumeric(sqlerrp, 8);
    }

    /** SQLERRD(n), 1-based as in COBOL. SQLERRD(3) holds the rows affected by INSERT/UPDATE/DELETE. */
    public int sqlerrd(int oneBasedIndex) {
        return sqlerrd[oneBasedIndex - 1];
    }

    public void setSqlerrd(int oneBasedIndex, int value) {
        sqlerrd[oneBasedIndex - 1] = value;
    }

    /** SQLWARN0..SQLWARNA, index 0..10. */
    public char sqlwarn(int index) {
        return sqlwarn[index];
    }

    public void setSqlwarn(int index, char value) {
        sqlwarn[index] = value;
    }

    public String sqlstate() {
        return sqlstate;
    }

    public void setSqlstate(String sqlstate) {
        this.sqlstate = Pic.alphanumeric(sqlstate, 5);
    }

    /** SQLCODE = ZERO. */
    public boolean isOk() {
        return sqlcode == SQLCODE_OK;
    }

    /** SQLCODE = +100. */
    public boolean isNotFound() {
        return sqlcode == SQLCODE_NOT_FOUND;
    }

    /** SQLCODE &lt; 0. */
    public boolean isError() {
        return sqlcode < 0;
    }
}
