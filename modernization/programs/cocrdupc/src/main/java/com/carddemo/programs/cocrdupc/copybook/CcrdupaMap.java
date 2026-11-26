package com.carddemo.programs.cocrdupc.copybook;

import static com.carddemo.programs.cocrdupc.Cobol.alnum;
import static com.carddemo.programs.cocrdupc.Cobol.lowValues;

import java.util.EnumMap;
import java.util.Map;

/**
 * COCRDUP (cpy-bms): symbolic map CCRDUPAI / CCRDUPAO for map CCRDUPA in mapset COCRDUP. Input
 * (I) and output (O) share storage, as the COBOL REDEFINES does. Per field: L (length / cursor
 * when -1), F/A (attribute), C (colour), P, H, V and the data.
 */
public final class CcrdupaMap {

    public enum Field {
        TRNNAME(4), TITLE01(40), CURDATE(8), PGMNAME(8), TITLE02(40), CURTIME(8),
        ACCTSID(11), CARDSID(16), CRDNAME(50), CRDSTCD(1), EXPMON(2), EXPYEAR(4), EXPDAY(2),
        INFOMSG(40), ERRMSG(80), FKEYS(21), FKEYSC(18);

        private final int length;

        Field(int length) {
            this.length = length;
        }

        public int length() {
            return length;
        }
    }

    /** One symbolic-map field. */
    public static final class MapField {
        private final Field field;
        private short length;
        private int attribute;
        private int color;
        private int ps;
        private int hilight;
        private int validn;
        private String data;

        MapField(Field field) {
            this.field = field;
            clear();
        }

        void clear() {
            length = 0;
            attribute = 0;
            color = 0;
            ps = 0;
            hilight = 0;
            validn = 0;
            data = lowValues(field.length());
        }

        public Field field() { return field; }
        public short length() { return length; }
        public void setLength(int v) { length = (short) v; }
        public int attribute() { return attribute; }
        public void setAttribute(int v) { attribute = v; }
        public int color() { return color; }
        public void setColor(int v) { color = v; }
        public int ps() { return ps; }
        public int hilight() { return hilight; }
        public int validn() { return validn; }
        public String data() { return data; }
        public void setData(String v) { data = alnum(v, field.length()); }
        /** MOVE -1 TO xxxL: SEND MAP ... CURSOR positions the cursor here. */
        public boolean hasCursor() { return length == -1; }
    }

    private final Map<Field, MapField> fields = new EnumMap<>(Field.class);

    public CcrdupaMap() {
        for (Field f : Field.values()) {
            fields.put(f, new MapField(f));
        }
    }

    public MapField get(Field f) {
        return fields.get(f);
    }

    /** MOVE LOW-VALUES TO CCRDUPAO. */
    public void clearToLowValues() {
        fields.values().forEach(MapField::clear);
    }
}
