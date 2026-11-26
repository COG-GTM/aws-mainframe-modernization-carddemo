package com.carddemo.programs.cousr01c.copybook;

import java.util.EnumMap;
import java.util.Map;
import java.util.Optional;

/**
 * COUSR01 symbolic map COUSR1AI / COUSR1AO (COUSR1AO REDEFINES COUSR1AI). Input and output views share
 * storage, so one {@link MapField} per BMS field holds the length (xxxL), attribute (xxxF/xxxA),
 * colour (xxxC), PS (xxxP), highlight (xxxH), validation (xxxV) and data (xxxI / xxxO).
 */
public final class Cousr1aMap {

    /** BMS fields in map order. */
    public enum Field {
        TRNNAME(4), TITLE01(40), CURDATE(8), PGMNAME(8), TITLE02(40), CURTIME(8),
        FNAME(20), LNAME(20), USERID(8), PASSWD(8), USRTYPE(1), ERRMSG(78);

        private final int length;

        Field(int length) {
            this.length = length;
        }

        public int length() {
            return length;
        }
    }

    /** One BMS field's slice of the symbolic map. */
    public static final class MapField {
        private final Field field;
        /** xxxL COMP PIC S9(4); -1 requests symbolic cursor positioning. */
        public short length;
        /** xxxF / xxxA. */
        public char attribute;
        /** xxxC colour. */
        public char color;
        /** xxxP programmed symbols. */
        public char ps;
        /** xxxH highlight. */
        public char hilight;
        /** xxxV validation. */
        public char validn;
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
            hilight = Pic.LOW_VALUE;
            validn = Pic.LOW_VALUE;
            data = Pic.lowValues(field.length());
        }

        /** xxxI / xxxO. */
        public String data() {
            return data;
        }

        /** {@code MOVE value TO xxxI / xxxO}. */
        public void data(String value) {
            data = Pic.x(value, field.length());
        }

        /** {@code MOVE LOW-VALUES TO xxxI}. */
        public void dataLowValues() {
            data = Pic.lowValues(field.length());
        }
    }

    private final Map<Field, MapField> fields = new EnumMap<>(Field.class);

    public Cousr1aMap() {
        for (Field f : Field.values()) {
            fields.put(f, new MapField(f));
        }
    }

    public MapField field(Field f) {
        return fields.get(f);
    }

    /** {@code MOVE LOW-VALUES TO COUSR1AO}. */
    public void lowValues() {
        fields.values().forEach(MapField::lowValues);
    }

    /** {@code MOVE -1 TO xxxL}. */
    public void cursor(Field f) {
        fields.get(f).length = -1;
    }

    /** SEND MAP ... CURSOR: BMS places the cursor on the first field (map order) whose length is -1. */
    public Optional<Field> symbolicCursor() {
        for (Field f : Field.values()) {
            if (fields.get(f).length == -1) {
                return Optional.of(f);
            }
        }
        return Optional.empty();
    }
}
