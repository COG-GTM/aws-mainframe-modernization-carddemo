package com.carddemo.programs.cousr01c.gateway;

/** DFHRESP condition codes that EXEC CICS WRITE DATASET can return. */
public enum CicsResp {
    NORMAL(0),
    FILENOTFOUND(12),
    NOTFND(13),
    DUPREC(14),
    DUPKEY(15),
    INVREQ(16),
    IOERR(17),
    NOSPACE(18),
    NOTOPEN(19),
    LENGERR(22),
    NOTAUTH(70),
    DISABLED(84);

    private final int code;

    CicsResp(int code) {
        this.code = code;
    }

    /** Numeric DFHRESP value (WS-RESP-CD). */
    public int code() {
        return code;
    }
}
