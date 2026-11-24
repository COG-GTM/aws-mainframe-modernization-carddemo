package com.carddemo.authfrds.domain;

/** Raised by the store adapter with the Db2 SQLCODE/SQLSTATE of a failed statement (SQLCA equivalent). */
public class SqlCodeException extends RuntimeException {

    private final int sqlCode;
    private final String sqlState;

    public SqlCodeException(int sqlCode, String sqlState, String message, Throwable cause) {
        super(message, cause);
        this.sqlCode = sqlCode;
        this.sqlState = sqlState;
    }

    public int sqlCode() {
        return sqlCode;
    }

    public String sqlState() {
        return sqlState;
    }
}
