package com.carddemo.programs.cocrdlic.copybook;

/** DFHAID - IBM-supplied EIBAID values (attention identifier of the key the operator pressed). */
public enum EibAid {
    NULL(Pic.LOW_VALUE), ENTER('\''), CLEAR('_'), CLRP('\u00a8'), PEN('='), OPID('W'), MSRE('X'),
    STRF('h'), TRIG('"'), PA1('%'), PA2('>'), PA3(','),
    PF1('1'), PF2('2'), PF3('3'), PF4('4'), PF5('5'), PF6('6'), PF7('7'), PF8('8'), PF9('9'),
    PF10(':'), PF11('#'), PF12('@'),
    PF13('A'), PF14('B'), PF15('C'), PF16('D'), PF17('E'), PF18('F'), PF19('G'), PF20('H'), PF21('I'),
    PF22('\u00a2'), PF23('.'), PF24('<');

    public final char code;

    EibAid(char code) {
        this.code = code;
    }
}
