package com.carddemo.programs.cocrdupc;

/** DFHRESP condition codes returned by the file control gateway. */
public enum CicsResp {
    NORMAL(0), ERROR(1), DUPREC(14), NOTFND(13), INVREQ(16), IOERR(17), NOTOPEN(19),
    LENGERR(22), DISABLED(84), LOCKED(100), RECORDBUSY(101);

    private final int code;

    CicsResp(int code) {
        this.code = code;
    }

    public int code() {
        return code;
    }
}
