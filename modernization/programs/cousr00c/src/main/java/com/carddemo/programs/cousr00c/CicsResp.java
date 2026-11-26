package com.carddemo.programs.cousr00c;

/** DFHRESP condition values used by the file-control calls. */
public enum CicsResp {
    NORMAL(0, null), FILENOTFOUND(12, "AEIL"), NOTFND(13, "AEIM"), INVREQ(16, "AEIP"), IOERR(17, "AEIQ"), NOTOPEN(19, "AEIS"),
    ENDFILE(20, "AEIT"), LENGERR(22, "AEIV"), MAPFAIL(36, "AEI9"), DISABLED(84, "AEXL");

    private final int code;
    private final String abendCode;

    CicsResp(int code, String abendCode) {
        this.code = code;
        this.abendCode = abendCode;
    }

    /** Task abend code CICS raises when this condition occurs on a command without RESP / HANDLE. */
    public String abendCode() {
        return abendCode;
    }

    public int code() {
        return code;
    }
}
