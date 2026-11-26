package com.carddemo.programs.cotrn02c.copybook;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * COTRN02 symbolic map {@code COTRN2AI} / {@code COTRN2AO} (REDEFINES). Each named field carries the
 * L (length, COMP S9(4)), F/A (flag/attribute), C/P/H/V (colour, PS, highlight, validation) bytes and the
 * shared I/O data area. The 12-byte TIOA prefix FILLER is not modelled.
 */
public final class Cotrn2aMap {

    /** Field names in map order with their I/O length (PIC X(n)). */
    public enum Field {
        TRNNAME(4), TITLE01(40), CURDATE(8), PGMNAME(8), TITLE02(40), CURTIME(8),
        ACTIDIN(11), CARDNIN(16), TTYPCD(2), TCATCD(4), TRNSRC(10), TDESC(60), TRNAMT(12),
        TORIGDT(10), TPROCDT(10), MID(9), MNAME(30), MCITY(25), MZIP(10), CONFIRM(1), ERRMSG(78);

        public final int length;

        Field(int length) {
            this.length = length;
        }
    }

    /** One symbolic-map field (xxxL, xxxF/xxxA, xxxC, xxxP, xxxH, xxxV, xxxI/xxxO). */
    public static final class MapField {
        private final Field field;
        private short length;
        private char attribute;
        private char color;
        private char ps;
        private char highlight;
        private char validation;
        private String data;

        MapField(Field field) {
            this.field = field;
            lowValues();
        }

        void lowValues() {
            length = 0;
            attribute = Pic.LOW_VALUE;
            color = Pic.LOW_VALUE;
            ps = Pic.LOW_VALUE;
            highlight = Pic.LOW_VALUE;
            validation = Pic.LOW_VALUE;
            data = Pic.lowValues(field.length);
        }

        public Field field() {
            return field;
        }

        public short getLength() {
            return length;
        }

        public void setLength(int v) {
            length = (short) v;
        }

        public char getAttribute() {
            return attribute;
        }

        public void setAttribute(char v) {
            attribute = v;
        }

        public char getColor() {
            return color;
        }

        public void setColor(char v) {
            color = v;
        }

        public char getPs() {
            return ps;
        }

        public char getHighlight() {
            return highlight;
        }

        public char getValidation() {
            return validation;
        }

        /** xxxI / xxxO (shared storage). */
        public String get() {
            return data;
        }

        /** Alphanumeric MOVE into xxxI / xxxO. */
        public void set(String v) {
            data = Pic.x(v, field.length);
        }

        /** MOVE SPACES TO xxxI. */
        public void spaces() {
            data = Pic.spaces(field.length);
        }

        void setRaw(String v) {
            data = v;
        }
    }

    private final Map<Field, MapField> fields = new LinkedHashMap<>();

    public Cotrn2aMap() {
        for (Field f : Field.values()) {
            fields.put(f, new MapField(f));
        }
    }

    public MapField f(Field field) {
        return fields.get(field);
    }

    public String get(Field field) {
        return fields.get(field).get();
    }

    public void set(Field field, String value) {
        fields.get(field).set(value);
    }

    /** {@code MOVE -1 TO xxxL}: request the cursor on this field when sent with CURSOR. */
    public void cursor(Field field) {
        fields.get(field).setLength(-1);
    }

    /** {@code MOVE LOW-VALUES TO COTRN2AO}. */
    public void lowValues() {
        fields.values().forEach(MapField::lowValues);
    }

    /**
     * BMS RECEIVE MAP INTO(COTRN2AI): fields the terminal did not transmit arrive as LOW-VALUES with L=0;
     * transmitted data is left-justified and space-padded, with L = number of characters received.
     */
    public void receive(Field field, String value) {
        MapField mf = fields.get(field);
        mf.lowValues();
        if (value != null) {
            String v = value.length() > field.length ? value.substring(0, field.length) : value;
            mf.setRaw(Pic.x(v, field.length));
            mf.setLength(v.length());
        }
    }

    /** SEND MAP ... CURSOR: the first field in map order whose L is -1, or null (default cursor). */
    public Field cursorField() {
        for (MapField mf : fields.values()) {
            if (mf.getLength() == -1) {
                return mf.field();
            }
        }
        return null;
    }
}
