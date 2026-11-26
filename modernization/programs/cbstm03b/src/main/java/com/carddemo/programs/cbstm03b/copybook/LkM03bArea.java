package com.carddemo.programs.cbstm03b.copybook;

/**
 * LINKAGE SECTION 01 LK-M03B-AREA (passed BY REFERENCE, so mutable; also WS-M03B-AREA in CBSTM03A).
 * <pre>
 * 05 LK-M03B-DD       PIC X(08).
 * 05 LK-M03B-OPER     PIC X(01).   88s: see {@link M03bOper}
 * 05 LK-M03B-RC       PIC X(02).
 * 05 LK-M03B-KEY      PIC X(25).
 * 05 LK-M03B-KEY-LN   PIC S9(4).
 * 05 LK-M03B-FLDT     PIC X(1000).
 * </pre>
 */
public final class LkM03bArea {

    public static final int DD_LEN = 8;
    public static final int RC_LEN = 2;
    public static final int KEY_LEN = 25;
    public static final int KEY_LN_DIGITS = 4;
    public static final int FLDT_LEN = 1000;
    /** 8 + 1 + 2 + 25 + 4 + 1000. */
    public static final int AREA_LENGTH = 1040;

    private String dd = Pic.alphanumeric("", DD_LEN);
    private char oper = ' ';
    private String rc = Pic.alphanumeric("", RC_LEN);
    private String key = Pic.alphanumeric("", KEY_LEN);
    private int keyLn;
    private String fldt = Pic.alphanumeric("", FLDT_LEN);

    public String dd() {
        return dd;
    }

    public LkM03bArea dd(String value) {
        this.dd = Pic.alphanumeric(value, DD_LEN);
        return this;
    }

    public char oper() {
        return oper;
    }

    public LkM03bArea oper(char value) {
        this.oper = value;
        return this;
    }

    /** SET M03B-xxx TO TRUE. */
    public LkM03bArea set(M03bOper value) {
        this.oper = value.code();
        return this;
    }

    public boolean is(M03bOper value) {
        return value.matches(oper);
    }

    public boolean isM03bOpen() {
        return is(M03bOper.OPEN);
    }

    public boolean isM03bClose() {
        return is(M03bOper.CLOSE);
    }

    public boolean isM03bRead() {
        return is(M03bOper.READ);
    }

    public boolean isM03bReadK() {
        return is(M03bOper.READ_K);
    }

    public boolean isM03bWrite() {
        return is(M03bOper.WRITE);
    }

    public boolean isM03bRewrite() {
        return is(M03bOper.REWRITE);
    }

    public String rc() {
        return rc;
    }

    public LkM03bArea rc(String value) {
        this.rc = Pic.alphanumeric(value, RC_LEN);
        return this;
    }

    public String key() {
        return key;
    }

    public LkM03bArea key(String value) {
        this.key = Pic.alphanumeric(value, KEY_LEN);
        return this;
    }

    public int keyLn() {
        return keyLn;
    }

    /** MOVE to PIC S9(4): high-order digits beyond 4 are truncated, sign kept. */
    public LkM03bArea keyLn(int value) {
        this.keyLn = value % 10_000;
        return this;
    }

    public String fldt() {
        return fldt;
    }

    public LkM03bArea fldt(String value) {
        this.fldt = Pic.alphanumeric(value, FLDT_LEN);
        return this;
    }
}
