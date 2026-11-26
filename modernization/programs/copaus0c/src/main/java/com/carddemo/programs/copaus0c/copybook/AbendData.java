package com.carddemo.programs.copaus0c.copybook;

/** CSMSG02Y - ABEND-DATA. COPYed by COPAUS0C but not referenced. */
public record AbendData(
        String abendCode,     // ABEND-CODE    PIC X(4)
        String abendCulprit,  // ABEND-CULPRIT PIC X(8)
        String abendReason,   // ABEND-REASON  PIC X(50)
        String abendMsg) {    // ABEND-MSG     PIC X(72)

    public static AbendData initial() {
        return new AbendData(" ".repeat(4), " ".repeat(8), " ".repeat(50), " ".repeat(72));
    }
}
