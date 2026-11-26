package com.carddemo.programs.cousr00c;

/** DFHRESP condition values used by the file-control calls. */
public enum CicsResp {
    NORMAL(0), NOTFND(13), INVREQ(16), IOERR(17), NOTOPEN(19), ENDFILE(20), LENGERR(22), MAPFAIL(36),
    DISABLED(84);

    private final int code;

    CicsResp(int code) {
        this.code = code;
    }

    public int code() {
        return code;
    }
}
