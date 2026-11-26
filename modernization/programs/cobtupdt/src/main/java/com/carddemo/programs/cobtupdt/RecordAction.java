package com.carddemo.programs.cobtupdt;

import java.util.Optional;

/** Values of INPUT-REC-TYPE handled by 1003-TREAT-RECORD (anything else is 'ERROR: TYPE NOT VALID'). */
public enum RecordAction {
    ADD('A'),
    UPDATE('U'),
    DELETE('D'),
    COMMENT('*');

    private final char code;

    RecordAction(char code) {
        this.code = code;
    }

    public char code() {
        return code;
    }

    public static Optional<RecordAction> fromCode(String inputRecType) {
        if (inputRecType == null || inputRecType.length() != 1) {
            return Optional.empty();
        }
        char c = inputRecType.charAt(0);
        for (RecordAction a : values()) {
            if (a.code == c) {
                return Optional.of(a);
            }
        }
        return Optional.empty();
    }
}
