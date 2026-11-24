package com.carddemo.transactiontype.domain;

/** Db2 SQLCODEs the legacy programs branch on. */
public final class SqlCodes {

    public static final int NOT_FOUND = 100;
    public static final int DUPLICATE_KEY = -803;
    public static final int DEADLOCK_OR_TIMEOUT = -911;
    public static final int DELETE_RESTRICTED_BY_FK = -532;

    private SqlCodes() {
    }
}
