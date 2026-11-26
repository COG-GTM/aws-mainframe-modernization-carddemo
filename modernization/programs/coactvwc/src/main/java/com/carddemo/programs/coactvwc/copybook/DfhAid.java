package com.carddemo.programs.coactvwc.copybook;

/** IBM DFHAID: attention identifiers (EIBAID values; EBCDIC byte shown for reference). */
public enum DfhAid {
    DFHNULL(0x00), DFHENTER(0x7D), DFHCLEAR(0x6D), DFHCLRP(0x6A), DFHPEN(0x7E), DFHOPID(0xE6), DFHMSRE(0xE7),
    DFHSTRF(0x88), DFHTRIG(0x7F), DFHPA1(0x6C), DFHPA2(0x6E), DFHPA3(0x6B),
    DFHPF1(0xF1), DFHPF2(0xF2), DFHPF3(0xF3), DFHPF4(0xF4), DFHPF5(0xF5), DFHPF6(0xF6),
    DFHPF7(0xF7), DFHPF8(0xF8), DFHPF9(0xF9), DFHPF10(0x7A), DFHPF11(0x7B), DFHPF12(0x7C),
    DFHPF13(0xC1), DFHPF14(0xC2), DFHPF15(0xC3), DFHPF16(0xC4), DFHPF17(0xC5), DFHPF18(0xC6),
    DFHPF19(0xC7), DFHPF20(0xC8), DFHPF21(0xC9), DFHPF22(0x4A), DFHPF23(0x4B), DFHPF24(0x4C);

    private final int ebcdic;

    DfhAid(int ebcdic) {
        this.ebcdic = ebcdic;
    }

    public int ebcdic() {
        return ebcdic;
    }
}
