package com.carddemo.programs.coactupc.gateway;

/** DFHRESP(...) condition values returned in RESP by CICS file control. */
public final class CicsResp {

    public static final int NORMAL = 0;
    public static final int NOTFND = 13;
    public static final int DUPREC = 14;
    public static final int DUPKEY = 15;
    public static final int INVREQ = 16;
    public static final int IOERR = 17;
    public static final int NOTOPEN = 19;
    public static final int LENGERR = 22;
    public static final int LOCKED = 100;
    public static final int DISABLED = 84;

    private CicsResp() {
    }
}
