package com.carddemo.runtime.cics;

/** EXEC CICS RESP condition values (DFHRESP). */
public enum CicsResp {
    NORMAL(0), ERROR(1), EOF(4), FILENOTFOUND(12), NOTFND(13), DUPREC(14), DUPKEY(15), INVREQ(16), IOERR(17),
    NOSPACE(18), NOTOPEN(19), ENDFILE(20), ILLOGIC(21), LENGERR(22), QZERO(23), ITEMERR(26), PGMIDERR(27),
    TRANSIDERR(28), MAPFAIL(36), NOTAUTH(70), DISABLED(84), RECORDBUSY(101);

    private final int code;

    CicsResp(int code) {
        this.code = code;
    }

    /** Numeric value as seen by {@code RESP(WS-RESP-CD)} / {@code EVALUATE WS-RESP-CD}. */
    public int code() {
        return code;
    }

    public static CicsResp of(int code) {
        for (CicsResp r : values()) {
            if (r.code == code) {
                return r;
            }
        }
        throw new IllegalArgumentException("Unknown RESP code " + code);
    }
}
