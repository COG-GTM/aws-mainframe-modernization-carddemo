package com.carddemo.programs.cocrdlic;

/** DFHRESP condition values returned by the file-control gateway. */
public final class CicsResp {
    public static final int NORMAL = 0;
    public static final int NOTFND = 13;
    public static final int DUPREC = 14;
    public static final int INVREQ = 16;
    public static final int IOERR = 17;
    public static final int NOTOPEN = 19;
    public static final int ENDFILE = 20;
    public static final int DISABLED = 84;

    private CicsResp() {
    }
}
