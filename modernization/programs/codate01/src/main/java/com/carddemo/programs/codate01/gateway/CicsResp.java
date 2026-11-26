package com.carddemo.programs.codate01.gateway;

/** DFHRESP values CODATE01 can see from EXEC CICS RETRIEVE. */
public final class CicsResp {

    private CicsResp() {
    }

    public static final int NORMAL = 0;
    public static final int NOTFND = 13;
    public static final int INVREQ = 16;
    public static final int LENGERR = 22;
    public static final int ENVDEFERR = 56;
}
