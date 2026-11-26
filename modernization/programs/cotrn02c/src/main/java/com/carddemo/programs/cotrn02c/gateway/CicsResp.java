package com.carddemo.programs.cotrn02c.gateway;

/** EXEC CICS RESP condition values (DFHRESP) used by COTRN02C. */
public enum CicsResp {
    NORMAL(0), FILENOTFOUND(12), NOTFND(13), DUPREC(14), DUPKEY(15), INVREQ(16), IOERR(17), NOSPACE(18),
    NOTOPEN(19), ENDFILE(20), ILLOGIC(21), LENGERR(22), SYSIDERR(53), ISCINVREQ(54), NOTAUTH(70),
    DISABLED(84), LOADING(94), LOCKED(100), RECORDBUSY(101);

    public final int code;

    CicsResp(int code) {
        this.code = code;
    }
}
