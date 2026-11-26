package com.carddemo.programs.coactvwc.repository;

/** DFHRESP values returned by EXEC CICS READ that COACTVWC distinguishes (others go to WHEN OTHER). */
public final class CicsResp {
    public static final int NORMAL = 0;
    public static final int NOTFND = 13;
    public static final int DUPKEY = 15;
    public static final int NOTOPEN = 19;
    public static final int DISABLED = 84;

    private CicsResp() {
    }
}
