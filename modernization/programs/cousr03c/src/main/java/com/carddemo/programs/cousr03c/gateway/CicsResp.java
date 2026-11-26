package com.carddemo.programs.cousr03c.gateway;

/** EXEC CICS RESP/RESP2 pair. Constants are the DFHRESP() values. */
public record CicsResp(int resp, int resp2) {
    public static final int NORMAL = 0;
    public static final int NOTFND = 13;
    public static final int DUPREC = 14;
    public static final int INVREQ = 16;
    public static final int IOERR = 17;
    public static final int NOTOPEN = 19;
    public static final int LENGERR = 22;
    public static final int MAPFAIL = 36;
    public static final int DISABLED = 84;

    public static CicsResp of(int resp) {
        return new CicsResp(resp, 0);
    }

    public boolean isNormal() {
        return resp == NORMAL;
    }
}
