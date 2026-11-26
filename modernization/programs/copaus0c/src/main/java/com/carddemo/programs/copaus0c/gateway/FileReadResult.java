package com.carddemo.programs.copaus0c.gateway;

/** Result of EXEC CICS READ ... RESP(WS-RESP-CD) RESP2(WS-REAS-CD) INTO(record). */
public record FileReadResult<T>(int resp, int resp2, T record) {

    /** DFHRESP(NORMAL). */
    public static final int NORMAL = 0;
    /** DFHRESP(NOTFND). */
    public static final int NOTFND = 13;
    /** RESP2 for NOTFND on READ. */
    public static final int NOTFND_RESP2 = 80;

    public static <T> FileReadResult<T> normal(T record) {
        return new FileReadResult<>(NORMAL, 0, record);
    }

    public static <T> FileReadResult<T> notFound() {
        return new FileReadResult<>(NOTFND, NOTFND_RESP2, null);
    }
}
