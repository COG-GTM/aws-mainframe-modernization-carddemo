package com.carddemo.programs.cotrn02c.gateway;

/** EXEC CICS RESP condition values (DFHRESP) used by COTRN02C. */
public enum CicsResp {
    NORMAL(0), NOTFND(13), DUPREC(14), DUPKEY(15), INVREQ(16), IOERR(17), NOSPACE(18), NOTOPEN(19),
    ENDFILE(20), LENGERR(22), DISABLED(84);

    public final int code;

    CicsResp(int code) {
        this.code = code;
    }
}
