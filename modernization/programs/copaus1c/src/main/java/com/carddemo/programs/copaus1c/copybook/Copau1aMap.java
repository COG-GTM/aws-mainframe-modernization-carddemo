package com.carddemo.programs.copaus1c.copybook;

import java.util.EnumMap;
import java.util.Map;

/**
 * COPAU01 (cpy-bms) - symbolic map COPAU1AI / COPAU1AO (REDEFINES) for map COPAU1A, mapset COPAU01.
 * Each field carries its data (xxxI / xxxO), length/cursor (xxxL), attribute (xxxA) and colour (xxxC).
 */
public final class Copau1aMap {

    /** Named fields of COPAU1A in map order, with their PIC X(n) lengths. */
    public enum Field {
        TRNNAME(4), TITLE01(40), CURDATE(8), PGMNAME(8), TITLE02(40), CURTIME(8),
        CARDNUM(16), AUTHDT(10), AUTHTM(10), AUTHRSP(1), AUTHRSN(20), AUTHCD(6), AUTHAMT(12),
        POSEMD(4), AUTHSRC(10), MCCCD(4), CRDEXP(5), AUTHTYP(14), TRNID(15), AUTHMTC(1), AUTHFRD(10),
        MERNAME(25), MERID(15), MERCITY(25), MERST(2), MERZIP(10), ERRMSG(78);

        public final int length;

        Field(int length) {
            this.length = length;
        }
    }

    private final EnumMap<Field, String> data = new EnumMap<>(Field.class);
    private final EnumMap<Field, Integer> lengths = new EnumMap<>(Field.class);
    private final EnumMap<Field, Character> attributes = new EnumMap<>(Field.class);
    private final EnumMap<Field, Character> colors = new EnumMap<>(Field.class);

    private Copau1aMap() {
    }

    /** MOVE LOW-VALUES TO COPAU1AO (also the state of the uninitialised map at task start). */
    public static Copau1aMap lowValues() {
        Copau1aMap m = new Copau1aMap();
        m.moveLowValues();
        return m;
    }

    public void moveLowValues() {
        for (Field f : Field.values()) {
            data.put(f, Cobol.lowValues(f.length));
            lengths.put(f, 0);
            attributes.put(f, Cobol.LOW_VALUE);
            colors.put(f, Cobol.LOW_VALUE);
        }
    }

    /** MOVE value TO xxxO. */
    public void move(Field f, String value) {
        data.put(f, Cobol.x(value, f.length));
    }

    /** MOVE value TO xxxO(pos:len) - reference-modified move; {@code len} null means "to the end". */
    public void moveRef(Field f, int pos, Integer len, String value) {
        int n = len == null ? f.length - pos + 1 : len;
        data.put(f, Cobol.overlay(data.get(f), pos, Cobol.x(value, n)));
    }

    public String get(Field f) {
        return data.get(f);
    }

    /** MOVE -1 TO xxxL (cursor position request). */
    public void setLength(Field f, int value) {
        lengths.put(f, value);
    }

    public int getLength(Field f) {
        return lengths.get(f);
    }

    public void setColor(Field f, BmsColor color) {
        colors.put(f, color.code);
    }

    public char getColor(Field f) {
        return colors.get(f);
    }

    public char getAttribute(Field f) {
        return attributes.get(f);
    }

    public Map<Field, String> snapshot() {
        return new EnumMap<>(data);
    }
}
