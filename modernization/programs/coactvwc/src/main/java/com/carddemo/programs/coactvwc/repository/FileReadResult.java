package com.carddemo.programs.coactvwc.repository;

/** Outcome of an EXEC CICS READ ... RESP(WS-RESP-CD) RESP2(WS-REAS-CD): {@code record} is set when data came back. */
public record FileReadResult<T>(int resp, int resp2, T record) {

    public static <T> FileReadResult<T> normal(T record) {
        return new FileReadResult<>(CicsResp.NORMAL, 0, record);
    }

    public static <T> FileReadResult<T> notFound() {
        return new FileReadResult<>(CicsResp.NOTFND, 0, null);
    }

    public static <T> FileReadResult<T> error(int resp, int resp2) {
        return new FileReadResult<>(resp, resp2, null);
    }
}
