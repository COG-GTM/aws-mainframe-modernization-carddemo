package com.carddemo.runtime.cics;

/** EXEC Interface Block fields used by CardDemo programs. */
public final class Eib {

    private final String eibtrnid;
    private final String eibtrmid;
    private final AidKey eibaid;
    private final int eibcalen;
    private int eibresp;
    private int eibresp2;

    public Eib(String eibtrnid, String eibtrmid, AidKey eibaid, int eibcalen) {
        this.eibtrnid = eibtrnid;
        this.eibtrmid = eibtrmid;
        this.eibaid = eibaid;
        this.eibcalen = eibcalen;
    }

    public String eibtrnid() {
        return eibtrnid;
    }

    public String eibtrmid() {
        return eibtrmid;
    }

    public AidKey eibaid() {
        return eibaid;
    }

    /** Length of the commarea passed to this task (0 on the first invocation of a pseudo-conversation). */
    public int eibcalen() {
        return eibcalen;
    }

    public int eibresp() {
        return eibresp;
    }

    public int eibresp2() {
        return eibresp2;
    }

    void setResp(int resp, int resp2) {
        this.eibresp = resp;
        this.eibresp2 = resp2;
    }
}
