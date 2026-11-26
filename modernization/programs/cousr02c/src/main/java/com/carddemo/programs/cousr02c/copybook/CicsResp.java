package com.carddemo.programs.cousr02c.copybook;

/** DFHRESP(...) values (EIBRESP) relevant to file control. */
public final class CicsResp {

    public static final int NORMAL = 0;
    public static final int ERROR = 1;
    public static final int FILENOTFOUND = 12;
    public static final int NOTFND = 13;
    public static final int DUPREC = 14;
    public static final int INVREQ = 16;
    public static final int IOERR = 17;
    public static final int NOSPACE = 18;
    public static final int NOTOPEN = 19;
    public static final int LENGERR = 22;
    public static final int MAPFAIL = 36;
    public static final int NOTAUTH = 70;
    public static final int DISABLED = 84;
    public static final int LOCKED = 100;

    private CicsResp() {
    }
}
