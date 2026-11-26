package com.carddemo.programs.cbstm03b.copybook;

import java.util.Optional;

/** 88-level values of LK-M03B-OPER. */
public enum M03bOper {
    /** 88 M03B-OPEN VALUE 'O'. */
    OPEN('O'),
    /** 88 M03B-CLOSE VALUE 'C'. */
    CLOSE('C'),
    /** 88 M03B-READ VALUE 'R'. */
    READ('R'),
    /** 88 M03B-READ-K VALUE 'K'. */
    READ_K('K'),
    /** 88 M03B-WRITE VALUE 'W'. */
    WRITE('W'),
    /** 88 M03B-REWRITE VALUE 'Z'. */
    REWRITE('Z');

    private final char code;

    M03bOper(char code) {
        this.code = code;
    }

    public char code() {
        return code;
    }

    public boolean matches(char value) {
        return value == code;
    }

    public static Optional<M03bOper> fromCode(char value) {
        for (M03bOper op : values()) {
            if (op.code == value) {
                return Optional.of(op);
            }
        }
        return Optional.empty();
    }
}
