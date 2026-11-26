package com.carddemo.programs.cbpaup0c;

import com.carddemo.programs.cbpaup0c.copybook.CobolCodec;

/** PRM-INFO: the 17-byte SYSIN control card accepted by 1000-INITIALIZE. */
public final class PrmInfo {

    public static final int LENGTH = 17;

    private String expiryDays;    // P-EXPIRY-DAYS   PIC 9(02) @0
    private final String filler1; // FILLER          PIC X(01) @2
    private String chkpFreq;      // P-CHKP-FREQ     PIC X(05) @3
    private final String filler2; // FILLER          PIC X(01) @8
    private String chkpDisFreq;   // P-CHKP-DIS-FREQ PIC X(05) @9
    private final String filler3; // FILLER          PIC X(01) @14
    private String debugFlag;     // P-DEBUG-FLAG    PIC X(01) @15
    private final String filler4; // FILLER          PIC X(01) @16

    private PrmInfo(String card) {
        expiryDays = card.substring(0, 2);
        filler1 = card.substring(2, 3);
        chkpFreq = card.substring(3, 8);
        filler2 = card.substring(8, 9);
        chkpDisFreq = card.substring(9, 14);
        filler3 = card.substring(14, 15);
        debugFlag = card.substring(15, 16);
        filler4 = card.substring(16, 17);
    }

    /**
     * ACCEPT PRM-INFO FROM SYSIN: the record is moved left-justified into the 01-level group, so it is
     * truncated or space-padded to {@link #LENGTH}. A missing record is treated as all spaces.
     */
    public static PrmInfo fromSysin(String record) {
        return new PrmInfo(CobolCodec.fit(record, LENGTH));
    }

    public String expiryDays() { return expiryDays; }
    public String chkpFreq() { return chkpFreq; }
    public String chkpDisFreq() { return chkpDisFreq; }
    public String debugFlag() { return debugFlag; }

    void chkpFreq(String v) { chkpFreq = CobolCodec.fit(v, 5); }
    void chkpDisFreq(String v) { chkpDisFreq = CobolCodec.fit(v, 5); }
    void debugFlag(String v) { debugFlag = CobolCodec.fit(v, 1); }

    /** 88 DEBUG-ON VALUE 'Y'. */
    public boolean isDebugOn() {
        return "Y".equals(debugFlag);
    }

    /** 88 DEBUG-OFF VALUE 'N'. */
    public boolean isDebugOff() {
        return "N".equals(debugFlag);
    }

    /** Raw 17 bytes, as DISPLAY PRM-INFO shows them. */
    @Override
    public String toString() {
        return expiryDays + filler1 + chkpFreq + filler2 + chkpDisFreq + filler3 + debugFlag + filler4;
    }
}
