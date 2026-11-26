package com.carddemo.programs.coactvwc;

import com.carddemo.programs.coactvwc.support.Cobol;

/** WS-THIS-PROGCOMMAREA: the 12 bytes COACTVWC appends after CARDDEMO-COMMAREA. */
public final class ThisProgCommarea {
    public static final int LENGTH = 12;

    private String caFromProgram; // CA-FROM-PROGRAM X(08)
    private String caFromTranid;  // CA-FROM-TRANID  X(04)

    private ThisProgCommarea(String raw) {
        String s = Cobol.alnum(raw == null ? "" : raw, LENGTH);
        caFromProgram = s.substring(0, 8);
        caFromTranid = s.substring(8, 12);
    }

    public static ThisProgCommarea fromString(String raw) {
        return new ThisProgCommarea(raw);
    }

    /** Fresh working storage (no VALUE clause). */
    public static ThisProgCommarea lowValues() {
        return new ThisProgCommarea(Cobol.lowValues(LENGTH));
    }

    /** INITIALIZE WS-THIS-PROGCOMMAREA. */
    public static ThisProgCommarea initialized() {
        return new ThisProgCommarea(Cobol.spaces(LENGTH));
    }

    public String caFromProgram() { return caFromProgram; }
    public void setCaFromProgram(String v) { caFromProgram = Cobol.alnum(v, 8); }
    public String caFromTranid() { return caFromTranid; }
    public void setCaFromTranid(String v) { caFromTranid = Cobol.alnum(v, 4); }

    @Override
    public String toString() {
        return caFromProgram + caFromTranid;
    }
}
