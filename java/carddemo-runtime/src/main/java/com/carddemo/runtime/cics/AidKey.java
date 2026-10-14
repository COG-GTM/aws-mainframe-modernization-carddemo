package com.carddemo.runtime.cics;

import java.util.Locale;

/** Attention identifiers (EIBAID / DFHAID). {@link #ebcdic} is the 3270 AID byte. */
public enum AidKey {
    ENTER(0x7D), CLEAR(0x6D), PA1(0x6C), PA2(0x6E), PA3(0x6B),
    PF1(0xF1), PF2(0xF2), PF3(0xF3), PF4(0xF4), PF5(0xF5), PF6(0xF6), PF7(0xF7), PF8(0xF8), PF9(0xF9),
    PF10(0x7A), PF11(0x7B), PF12(0x7C),
    PF13(0xC1), PF14(0xC2), PF15(0xC3), PF16(0xC4), PF17(0xC5), PF18(0xC6), PF19(0xC7), PF20(0xC8), PF21(0xC9),
    PF22(0x4A), PF23(0x4B), PF24(0x4C);

    private final int ebcdic;

    AidKey(int ebcdic) {
        this.ebcdic = ebcdic;
    }

    public int ebcdic() {
        return ebcdic;
    }

    /** Parses "ENTER", "PF3", "pf03", "F3", "CLEAR" etc. */
    public static AidKey parse(String text) {
        String t = text.trim().toUpperCase(Locale.ROOT);
        if (t.matches("F\\d{1,2}")) {
            t = "P" + t;
        }
        if (t.matches("PF0\\d")) {
            t = "PF" + t.substring(3);
        }
        return valueOf(t);
    }
}
