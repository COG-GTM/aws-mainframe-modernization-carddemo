package com.carddemo.programs.cbstm03a.copybook;

import com.carddemo.programs.cbstm03a.support.Pic;

/**
 * WS-M03B-AREA (CBSTM03A WORKING-STORAGE) / LK-M03B-AREA (CBSTM03B LINKAGE):
 * the parameter block passed on {@code CALL 'CBSTM03B' USING WS-M03B-AREA}.
 */
public final class M03bArea {

    public static final int FLDT_LENGTH = 1000;

    /** WS-M03B-OPER 88-levels. */
    public enum Oper {
        OPEN('O'),     // 88 M03B-OPEN
        CLOSE('C'),    // 88 M03B-CLOSE
        READ('R'),     // 88 M03B-READ
        READ_K('K'),   // 88 M03B-READ-K
        WRITE('W'),    // 88 M03B-WRITE
        REWRITE('Z');  // 88 M03B-REWRITE

        private final char code;

        Oper(char code) {
            this.code = code;
        }

        public char code() {
            return code;
        }
    }

    private String dd = Pic.x("", 8);          // WS-M03B-DD      PIC X(08)
    private Oper oper;                          // WS-M03B-OPER    PIC X(01)
    private String rc = Pic.x("", 2);          // WS-M03B-RC      PIC X(02)
    private String key = Pic.x("", 25);        // WS-M03B-KEY     PIC X(25)
    private int keyLn;                          // WS-M03B-KEY-LN  PIC S9(4)
    private String fldt = Pic.x("", FLDT_LENGTH); // WS-M03B-FLDT PIC X(1000)

    public String dd() {
        return dd;
    }

    public void dd(String value) {
        dd = Pic.x(value, 8);
    }

    public Oper oper() {
        return oper;
    }

    public void oper(Oper value) {
        oper = value;
    }

    public String rc() {
        return rc;
    }

    public void rc(String value) {
        rc = Pic.x(value, 2);
    }

    public String key() {
        return key;
    }

    public void key(String value) {
        key = Pic.x(value, 25);
    }

    public int keyLn() {
        return keyLn;
    }

    public void keyLn(int value) {
        keyLn = value % 10000;
    }

    public String fldt() {
        return fldt;
    }

    public void fldt(String value) {
        fldt = Pic.x(value, FLDT_LENGTH);
    }
}
