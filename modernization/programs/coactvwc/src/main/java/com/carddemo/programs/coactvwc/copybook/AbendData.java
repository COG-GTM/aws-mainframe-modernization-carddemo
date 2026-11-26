package com.carddemo.programs.coactvwc.copybook;

import com.carddemo.programs.coactvwc.support.Cobol;

/** CSMSG02Y: {@code ABEND-DATA}; all fields VALUE SPACES. */
public final class AbendData {
    private String abendCode = Cobol.spaces(4);     // X(4)
    private String abendCulprit = Cobol.spaces(8);  // X(8)
    private String abendReason = Cobol.spaces(50);  // X(50)
    private String abendMsg = Cobol.spaces(72);     // X(72)

    public String abendCode() { return abendCode; }
    public void setAbendCode(String v) { abendCode = Cobol.alnum(v, 4); }
    public String abendCulprit() { return abendCulprit; }
    public void setAbendCulprit(String v) { abendCulprit = Cobol.alnum(v, 8); }
    public String abendReason() { return abendReason; }
    public void setAbendReason(String v) { abendReason = Cobol.alnum(v, 50); }
    public String abendMsg() { return abendMsg; }
    public void setAbendMsg(String v) { abendMsg = Cobol.alnum(v, 72); }

    /** The 134-byte ABEND-DATA group as sent to the terminal. */
    @Override
    public String toString() {
        return abendCode + abendCulprit + abendReason + abendMsg;
    }
}
