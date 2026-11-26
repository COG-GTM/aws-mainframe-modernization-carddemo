package com.carddemo.programs.cocrdslc;

/**
 * WS-THIS-PROGCOMMAREA (12 bytes), appended after CARDDEMO-COMMAREA in this program's COMMAREA.
 * <pre>
 * 05 CA-CALL-CONTEXT.
 *    10 CA-FROM-PROGRAM  PIC X(08).
 *    10 CA-FROM-TRANID   PIC X(04).
 * </pre>
 */
public final class ThisProgCommarea {
    public static final int LENGTH = 12;

    public String caFromProgram;
    public String caFromTranid;

    public static ThisProgCommarea parse(String raw) {
        FixedWidthReader r = new FixedWidthReader(raw, LENGTH);
        ThisProgCommarea t = new ThisProgCommarea();
        t.caFromProgram = r.x(8);
        t.caFromTranid = r.x(4);
        return t;
    }

    public static ThisProgCommarea lowValues() {
        return parse(CobolText.lowValues(LENGTH));
    }

    public void initialize() {
        caFromProgram = CobolText.spaces(8);
        caFromTranid = CobolText.spaces(4);
    }

    public String toFixedWidth() {
        return CobolText.pic(caFromProgram, 8) + CobolText.pic(caFromTranid, 4);
    }
}
