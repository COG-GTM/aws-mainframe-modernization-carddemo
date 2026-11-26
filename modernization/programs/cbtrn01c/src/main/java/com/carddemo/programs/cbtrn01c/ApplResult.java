package com.carddemo.programs.cbtrn01c;

/** 01 APPL-RESULT PIC S9(9) COMP with its 88-levels. */
public final class ApplResult {

    public static final int AOK = 0;   // 88 APPL-AOK VALUE 0
    public static final int EOF = 16;  // 88 APPL-EOF VALUE 16

    private int value;

    public int value() {
        return value;
    }

    public void set(int newValue) {
        value = newValue;
    }

    public boolean isAok() {
        return value == AOK;
    }

    public boolean isEof() {
        return value == EOF;
    }
}
