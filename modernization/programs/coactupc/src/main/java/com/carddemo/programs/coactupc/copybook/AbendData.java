package com.carddemo.programs.coactupc.copybook;

import com.carddemo.programs.coactupc.support.Cobol;

/** CSMSG02Y - ABEND-DATA (all fields VALUE SPACES). */
public final class AbendData {

    /** ABEND-CODE PIC X(4). */
    public String abendCode = Cobol.spaces(4);
    /** ABEND-CULPRIT PIC X(8). */
    public String abendCulprit = Cobol.spaces(8);
    /** ABEND-REASON PIC X(50). */
    public String abendReason = Cobol.spaces(50);
    /** ABEND-MSG PIC X(72). */
    public String abendMsg = Cobol.spaces(72);

    public AbendData copy() {
        AbendData a = new AbendData();
        a.abendCode = abendCode;
        a.abendCulprit = abendCulprit;
        a.abendReason = abendReason;
        a.abendMsg = abendMsg;
        return a;
    }

    /** ABEND-DATA as the 134-byte block sent to the terminal. */
    public String toFixedWidth() {
        return Cobol.pad(abendCode, 4) + Cobol.pad(abendCulprit, 8) + Cobol.pad(abendReason, 50)
                + Cobol.pad(abendMsg, 72);
    }
}
