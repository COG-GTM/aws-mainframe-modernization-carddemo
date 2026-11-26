package com.carddemo.programs.corpt00c;

/** RESP / RESP2 pair returned by a CICS command. */
public record CicsResponse(int resp, int resp2) {

    /** DFHRESP(NORMAL). */
    public static final int NORMAL = 0;
    /** DFHRESP(IOERR). */
    public static final int IOERR = 17;
    /** DFHRESP(NOSPACE). */
    public static final int NOSPACE = 18;
    /** DFHRESP(NOTOPEN). */
    public static final int NOTOPEN = 19;
    /** DFHRESP(LENGERR). */
    public static final int LENGERR = 22;
    /** DFHRESP(QIDERR). */
    public static final int QIDERR = 44;
    /** DFHRESP(DISABLED). */
    public static final int DISABLED = 84;

    public static CicsResponse normal() {
        return new CicsResponse(NORMAL, 0);
    }

    public boolean isNormal() {
        return resp == NORMAL;
    }
}
