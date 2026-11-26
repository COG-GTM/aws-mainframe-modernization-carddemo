package com.carddemo.programs.cocrdslc;

/** DFHRESP values used by the CICS file-control gateway. */
public final class CicsResp {
    public static final int NORMAL = 0;
    public static final int NOTFND = 13;
    public static final int DUPKEY = 15;
    /** RESP2 for DUPKEY on READ: another alternate-index record with the same key follows. */
    public static final int DUPKEY_RESP2_MORE = 140;
    public static final int NOTOPEN = 19;
    public static final int DISABLED = 84;

    private CicsResp() {
    }
}
