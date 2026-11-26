package com.carddemo.programs.cotrtlic.gateway;

/** Outcome of one embedded SQL statement: the SQLCA fields COTRTLIC reads plus any INTO value. */
public record SqlResult<T>(int sqlcode, String sqlerrmc, T value) {

    public static final int OK = 0;
    public static final int NOT_FOUND = 100;

    public static <T> SqlResult<T> ok(T value) {
        return new SqlResult<>(OK, "", value);
    }

    public static <T> SqlResult<T> notFound() {
        return new SqlResult<>(NOT_FOUND, "", null);
    }

    public static <T> SqlResult<T> error(int sqlcode, String sqlerrmc) {
        return new SqlResult<>(sqlcode, sqlerrmc, null);
    }
}
