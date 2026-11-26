package com.carddemo.programs.cotrn01c;

/** DFHRESP(...) condition codes used by the TRANSACT file gateway. */
public final class CicsResp {

    public static final int NORMAL = 0;
    public static final int NOTFND = 13;
    public static final int NOTOPEN = 19;
    public static final int LENGERR = 22;
    public static final int DISABLED = 84;

    private CicsResp() {
    }
}
