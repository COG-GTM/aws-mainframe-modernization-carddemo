package com.carddemo.programs.cotrn00c;

/** DFHRESP condition values used by the TRANSACT browse, with the abend code raised when unhandled. */
public enum CicsResp {
    NORMAL(0, null),
    FILENOTFOUND(12, "AEIL"),
    NOTFND(13, "AEIM"),
    INVREQ(16, "AEIP"),
    IOERR(17, "AEIQ"),
    NOTOPEN(19, "AEIS"),
    ENDFILE(20, "AEIT"),
    ILLOGIC(21, "AEIU"),
    LENGERR(22, "AEIV"),
    DISABLED(84, "AEXL");

    private final int code;
    private final String abendCode;

    CicsResp(int code, String abendCode) {
        this.code = code;
        this.abendCode = abendCode;
    }

    public int code() {
        return code;
    }

    /** Abend code CICS raises when the condition occurs on a command without RESP / HANDLE. */
    public String abendCode() {
        return abendCode;
    }
}
