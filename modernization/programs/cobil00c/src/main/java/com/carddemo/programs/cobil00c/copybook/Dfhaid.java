package com.carddemo.programs.cobil00c.copybook;

/** IBM CICS DFHAID copybook: attention identifier values carried in EIBAID. */
public enum Dfhaid {
    DFHNULL('\u0000'), DFHENTER('\''), DFHCLEAR('_'), DFHCLRP('\u00A1'), DFHPEN('='), DFHOPID('W'),
    DFHMSRE('X'), DFHSTRF('h'), DFHTRIG('"'), DFHPA1('%'), DFHPA2('>'), DFHPA3(','),
    DFHPF1('1'), DFHPF2('2'), DFHPF3('3'), DFHPF4('4'), DFHPF5('5'), DFHPF6('6'),
    DFHPF7('7'), DFHPF8('8'), DFHPF9('9'), DFHPF10(':'), DFHPF11('#'), DFHPF12('@'),
    DFHPF13('A'), DFHPF14('B'), DFHPF15('C'), DFHPF16('D'), DFHPF17('E'), DFHPF18('F'),
    DFHPF19('G'), DFHPF20('H'), DFHPF21('I'), DFHPF22('\u00A2'), DFHPF23('.'), DFHPF24('<');

    private final char value;

    Dfhaid(char value) {
        this.value = value;
    }

    public char value() {
        return value;
    }
}
