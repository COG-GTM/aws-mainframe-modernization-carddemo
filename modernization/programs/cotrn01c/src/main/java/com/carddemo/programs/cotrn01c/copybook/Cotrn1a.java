package com.carddemo.programs.cotrn01c.copybook;

import java.util.EnumMap;
import java.util.Map;

/**
 * COTRN01 (app/cpy-bms/COTRN01.CPY) - symbolic map COTRN1AI with COTRN1AO REDEFINES COTRN1AI.
 * Input (xxxI) and output (xxxO) share storage, so one value per field is kept. The xxxL length
 * field is kept only for cursor positioning (MOVE -1 TO xxxL); xxxF/xxxA/C/P/H/V are not used
 * by COTRN01C.
 */
public final class Cotrn1a {

    /** Map fields in BMS order with their PIC X(n) widths. */
    public enum Field {
        TRNNAME(4), TITLE01(40), CURDATE(8), PGMNAME(8), TITLE02(40), CURTIME(8),
        TRNIDIN(16), TRNID(16), CARDNUM(16), TTYPCD(2), TCATCD(4), TRNSRC(10), TDESC(60), TRNAMT(12),
        TORIGDT(10), TPROCDT(10), MID(9), MNAME(30), MCITY(25), MZIP(10), ERRMSG(78);

        private final int length;

        Field(int length) {
            this.length = length;
        }

        public int length() {
            return length;
        }
    }

    private final Map<Field, String> values = new EnumMap<>(Field.class);
    private final Map<Field, Integer> lengths = new EnumMap<>(Field.class);

    public Cotrn1a() {
        moveLowValues();
    }

    /** MOVE LOW-VALUES TO COTRN1AO (whole map, including the length fields). */
    public void moveLowValues() {
        for (Field f : Field.values()) {
            values.put(f, Cobol.lowValues(f.length()));
            lengths.put(f, 0);
        }
    }

    public String get(Field f) {
        return values.get(f);
    }

    /** Alphanumeric MOVE into the field (space padded / right truncated to the field width). */
    public void move(Field f, String value) {
        values.put(f, Cobol.moveAlnum(value, f.length()));
    }

    /** Stores a raw value already sized to the field (e.g. low-values from RECEIVE MAP). */
    public void putRaw(Field f, String value) {
        if (value.length() != f.length()) {
            throw new IllegalArgumentException(f + " must be " + f.length() + " chars");
        }
        values.put(f, value);
    }

    /** MOVE -1 TO xxxL : request symbolic cursor positioning on this field. */
    public void setCursor(Field f) {
        lengths.put(f, -1);
    }

    public void setLength(Field f, int length) {
        lengths.put(f, length);
    }

    public int getLength(Field f) {
        return lengths.get(f);
    }

    /** SEND ... CURSOR: BMS places the cursor on the first field whose length is -1, else none. */
    public Field cursorField() {
        for (Field f : Field.values()) {
            if (lengths.get(f) == -1) {
                return f;
            }
        }
        return null;
    }

    public Map<Field, String> snapshot() {
        return Map.copyOf(values);
    }
}
