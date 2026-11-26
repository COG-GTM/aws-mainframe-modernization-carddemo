package com.carddemo.programs.cocrdslc.copybook;

import com.carddemo.programs.cocrdslc.CobolText;

/**
 * CSMSG02Y - ABEND-DATA (134 bytes). Every field has VALUE SPACES.
 *
 * <pre>
 * 05 ABEND-CODE     PIC X(4).
 * 05 ABEND-CULPRIT  PIC X(8).
 * 05 ABEND-REASON   PIC X(50).
 * 05 ABEND-MSG      PIC X(72).
 * </pre>
 */
public final class AbendData {
    public static final int LENGTH = 134;

    private String abendCode = CobolText.spaces(4);
    private String abendCulprit = CobolText.spaces(8);
    private String abendReason = CobolText.spaces(50);
    private String abendMsg = CobolText.spaces(72);

    public String abendCode() {
        return abendCode;
    }

    public void abendCode(String v) {
        abendCode = CobolText.pic(v, 4);
    }

    public String abendCulprit() {
        return abendCulprit;
    }

    public void abendCulprit(String v) {
        abendCulprit = CobolText.pic(v, 8);
    }

    public String abendReason() {
        return abendReason;
    }

    public void abendReason(String v) {
        abendReason = CobolText.pic(v, 50);
    }

    public String abendMsg() {
        return abendMsg;
    }

    public void abendMsg(String v) {
        abendMsg = CobolText.pic(v, 72);
    }

    public String toFixedWidth() {
        return abendCode + abendCulprit + abendReason + abendMsg;
    }
}
