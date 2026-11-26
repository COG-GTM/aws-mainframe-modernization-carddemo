package com.carddemo.programs.corpt00c;

/**
 * CSUTLDTC-RESULT PIC X(80) as laid out by CORPT00C:
 * SEV-CD X(04), FILLER X(11), MSG-NUM X(04), MSG X(61).
 */
public record CsutldtcResult(String raw) {

    public static final int LENGTH = 80;

    public CsutldtcResult {
        raw = CobolText.move(raw, LENGTH);
    }

    public String sevCd() {
        return raw.substring(0, 4);
    }

    public String msgNum() {
        return raw.substring(15, 19);
    }

    public String msg() {
        return raw.substring(19, 80);
    }
}
