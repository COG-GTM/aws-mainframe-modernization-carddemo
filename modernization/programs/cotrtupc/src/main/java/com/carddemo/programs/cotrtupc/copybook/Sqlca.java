package com.carddemo.programs.cotrtupc.copybook;

import com.carddemo.programs.cotrtupc.support.CobolText;

/** EXEC SQL INCLUDE SQLCA (fields used by COTRTUPC plus the standard header). */
public final class Sqlca {

    public static final int SQLERRMC_LENGTH = 70;

    public String sqlcaid = "SQLCA   ";
    public int sqlcabc = 136;
    /** SQLCODE PIC S9(9) COMP. */
    public int sqlcode;
    /** SQLERRML PIC S9(4) COMP. */
    public short sqlerrml;
    /** SQLERRMC PIC X(70). */
    public String sqlerrmc = CobolText.spaces(SQLERRMC_LENGTH);
    public String sqlerrp = CobolText.spaces(8);
    public final int[] sqlerrd = new int[6];
    public String sqlwarn = CobolText.spaces(11);
    public String sqlstate = "00000";

    public static Sqlca of(int sqlcode, String sqlerrmc, String sqlstate) {
        Sqlca s = new Sqlca();
        s.sqlcode = sqlcode;
        String m = sqlerrmc == null ? "" : sqlerrmc;
        if (m.length() > SQLERRMC_LENGTH) {
            m = m.substring(0, SQLERRMC_LENGTH);
        }
        s.sqlerrml = (short) m.length();
        s.sqlerrmc = CobolText.pic(m, SQLERRMC_LENGTH);
        s.sqlstate = sqlstate;
        return s;
    }

    public static Sqlca ok() {
        return of(0, "", "00000");
    }

    public static Sqlca notFound() {
        return of(100, "", "02000");
    }

    /**
     * SQLERRM OF SQLCA as a 72-byte group (used with STRING ... DELIMITED BY SIZE): the two raw bytes of
     * the SQLERRML halfword followed by SQLERRMC.
     */
    public String sqlerrmGroup() {
        char hi = (char) ((sqlerrml >> 8) & 0xFF);
        char lo = (char) (sqlerrml & 0xFF);
        return "" + hi + lo + sqlerrmc;
    }
}
