package com.carddemo.programs.copaus2c.copybook;

/**
 * SQLCA (EXEC SQL INCLUDE SQLCA) - the subset COPAUS2C reads: SQLCODE (S9(9) COMP) and SQLSTATE (X(5)).
 */
public record Sqlca(int sqlcode, String sqlstate) {

    public static final int DUPLICATE_KEY = -803;
    public static final int NOT_FOUND = 100;

    public static final Sqlca OK = new Sqlca(0, "00000");

    public Sqlca {
        sqlstate = Pic.x(sqlstate, 5);
    }
}
