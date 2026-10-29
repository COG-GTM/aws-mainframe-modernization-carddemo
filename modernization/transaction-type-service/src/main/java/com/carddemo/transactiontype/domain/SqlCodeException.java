package com.carddemo.transactiontype.domain;

/** Raised by the store adapter with the Db2 SQLCODE of a failed statement (SQLCA equivalent). */
public class SqlCodeException extends RuntimeException {

    private final int sqlCode;

    public SqlCodeException(int sqlCode, String message, Throwable cause) {
        super(message, cause);
        this.sqlCode = sqlCode;
    }

    public int sqlCode() {
        return sqlCode;
    }
}
