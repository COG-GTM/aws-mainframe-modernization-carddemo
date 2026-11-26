package com.carddemo.programs.copaus1c.copybook;

/** CSMSG02Y - ABEND-DATA work area (declared by COPAUS1C, not referenced by its logic). */
public final class AbendData {
    private String abendCode = Cobol.spaces(4);     // ABEND-CODE    X(04)
    private String abendCulprit = Cobol.spaces(8);  // ABEND-CULPRIT X(08)
    private String abendReason = Cobol.spaces(50);  // ABEND-REASON  X(50)
    private String abendMsg = Cobol.spaces(72);     // ABEND-MSG     X(72)

    public String getAbendCode() { return abendCode; }
    public void setAbendCode(String v) { abendCode = Cobol.x(v, 4); }
    public String getAbendCulprit() { return abendCulprit; }
    public void setAbendCulprit(String v) { abendCulprit = Cobol.x(v, 8); }
    public String getAbendReason() { return abendReason; }
    public void setAbendReason(String v) { abendReason = Cobol.x(v, 50); }
    public String getAbendMsg() { return abendMsg; }
    public void setAbendMsg(String v) { abendMsg = Cobol.x(v, 72); }
}
