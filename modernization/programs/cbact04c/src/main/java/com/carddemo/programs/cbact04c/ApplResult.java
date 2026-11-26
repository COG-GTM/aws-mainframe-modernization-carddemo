package com.carddemo.programs.cbact04c;

/**
 * APPL-RESULT PIC S9(9) COMP with its level-88 condition names.
 * <pre>
 * 88 APPL-AOK VALUE 0.
 * 88 APPL-EOF VALUE 16.
 * </pre>
 */
final class ApplResult {

    static final int AOK = 0;
    static final int EOF = 16;

    private int value;

    void set(int value) {
        this.value = value;
    }

    int value() {
        return value;
    }

    boolean isApplAok() {
        return value == AOK;
    }

    boolean isApplEof() {
        return value == EOF;
    }
}
