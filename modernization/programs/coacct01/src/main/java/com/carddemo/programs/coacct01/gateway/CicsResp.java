package com.carddemo.programs.coacct01.gateway;

/** DFHRESP(...) values used by or observable in COACCT01. */
public final class CicsResp {

    private CicsResp() {
    }

    public static final int NORMAL = 0;
    public static final int FILENOTFOUND = 12;
    public static final int NOTFND = 13;
    public static final int INVREQ = 16;
    public static final int IOERR = 17;
    public static final int NOTOPEN = 19;
    public static final int LENGERR = 22;
    public static final int ENDDATA = 29;
    public static final int DISABLED = 84;
}
