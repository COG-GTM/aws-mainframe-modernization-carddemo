package com.carddemo.programs.cocrdupc.copybook;

import static com.carddemo.programs.cocrdupc.Cobol.alnum;
import static com.carddemo.programs.cocrdupc.Cobol.spaces;

/** CSMSG02Y: 01 ABEND-DATA (134 bytes, every field VALUE SPACES). */
public final class AbendData {

    public static final int LENGTH = 134;

    private String abendCode = spaces(4);
    private String abendCulprit = spaces(8);
    private String abendReason = spaces(50);
    private String abendMsg = spaces(72);

    public String abendCode() { return abendCode; }
    public void setAbendCode(String v) { abendCode = alnum(v, 4); }
    public String abendCulprit() { return abendCulprit; }
    public void setAbendCulprit(String v) { abendCulprit = alnum(v, 8); }
    public String abendReason() { return abendReason; }
    public void setAbendReason(String v) { abendReason = alnum(v, 50); }
    public String abendMsg() { return abendMsg; }
    public void setAbendMsg(String v) { abendMsg = alnum(v, 72); }

    /** The 134-byte block sent to the terminal by ABEND-ROUTINE. */
    public String encode() {
        return abendCode + abendCulprit + abendReason + abendMsg;
    }

    public AbendData copy() {
        AbendData d = new AbendData();
        d.abendCode = abendCode;
        d.abendCulprit = abendCulprit;
        d.abendReason = abendReason;
        d.abendMsg = abendMsg;
        return d;
    }
}
