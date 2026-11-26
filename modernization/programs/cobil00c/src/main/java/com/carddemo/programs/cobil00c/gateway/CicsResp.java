package com.carddemo.programs.cobil00c.gateway;

/** EIBRESP values tested via DFHRESP(...) in COBIL00C, plus other common conditions. */
public enum CicsResp {
    NORMAL(0), NOTFND(13), DUPREC(14), DUPKEY(15), INVREQ(16), IOERR(17), NOSPACE(18), NOTOPEN(19),
    ENDFILE(20), ILLOGIC(21), LENGERR(22), MAPFAIL(36), DISABLED(84);

    private final int code;

    CicsResp(int code) {
        this.code = code;
    }

    public int code() {
        return code;
    }
}
