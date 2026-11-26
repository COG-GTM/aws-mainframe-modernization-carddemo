package com.carddemo.programs.comen01c.copybook;

import java.util.ArrayList;
import java.util.List;

/**
 * COMEN01 (app/cpy-bms/COMEN01.CPY) - symbolic map {@code COMEN1AI} with {@code COMEN1AO REDEFINES COMEN1AI}.
 *
 * <p>Each named field carries its input view (L length, F flag, I data) and output view (C colour,
 * P PS, H highlight, V validation, O data). The I and O data share storage, as in the REDEFINES.
 */
public final class Comen01Map {

    /** One BMS field: xxxL/xxxF/xxxI and xxxC/xxxP/xxxH/xxxV/xxxO. */
    public static final class MapField {
        private final String name;
        private final int size;
        private short length;      // xxxL COMP PIC S9(4)
        private char flag;         // xxxF / xxxA PIC X
        private char color;        // xxxC PIC X
        private char ps;           // xxxP PIC X
        private char hilight;      // xxxH PIC X
        private char validn;       // xxxV PIC X
        private String data;       // xxxI / xxxO

        MapField(String name, int size) {
            this.name = name;
            this.size = size;
            lowValues();
        }

        void lowValues() {
            length = 0;
            flag = CobolText.LOW_VALUE;
            color = CobolText.LOW_VALUE;
            ps = CobolText.LOW_VALUE;
            hilight = CobolText.LOW_VALUE;
            validn = CobolText.LOW_VALUE;
            data = CobolText.lowValues(size);
        }

        public String name() { return name; }
        public int size() { return size; }
        public short length() { return length; }
        public void length(short v) { length = v; }
        public char flag() { return flag; }
        public void flag(char v) { flag = v; }
        public char color() { return color; }
        public void color(char v) { color = v; }
        public char ps() { return ps; }
        public char hilight() { return hilight; }
        public char validn() { return validn; }

        /** xxxI / xxxO content. */
        public String data() { return data; }

        /** Alphanumeric MOVE into xxxI / xxxO. */
        public void move(String value) { data = CobolText.move(value, size); }

        /** Raw store of exactly {@code size} bytes (e.g. LOW-VALUES or data placed by RECEIVE MAP). */
        public void store(String raw) {
            if (raw.length() != size) {
                throw new IllegalArgumentException(name + " is " + size + " bytes");
            }
            data = raw;
        }
    }

    public static final int OPTION_LINES = 12;

    private final MapField trnname = new MapField("TRNNAME", 4);
    private final MapField title01 = new MapField("TITLE01", 40);
    private final MapField curdate = new MapField("CURDATE", 8);
    private final MapField pgmname = new MapField("PGMNAME", 8);
    private final MapField title02 = new MapField("TITLE02", 40);
    private final MapField curtime = new MapField("CURTIME", 8);
    private final List<MapField> optn = new ArrayList<>();
    private final MapField option = new MapField("OPTION", 2);
    private final MapField errmsg = new MapField("ERRMSG", 78);

    public Comen01Map() {
        for (int i = 1; i <= OPTION_LINES; i++) {
            optn.add(new MapField("OPTN%03d".formatted(i), 40));
        }
    }

    /** {@code MOVE LOW-VALUES TO COMEN1AO}. */
    public void lowValues() {
        all().forEach(MapField::lowValues);
    }

    public List<MapField> all() {
        List<MapField> fields = new ArrayList<>(List.of(trnname, title01, curdate, pgmname, title02, curtime));
        fields.addAll(optn);
        fields.add(option);
        fields.add(errmsg);
        return fields;
    }

    public MapField trnname() { return trnname; }
    public MapField title01() { return title01; }
    public MapField curdate() { return curdate; }
    public MapField pgmname() { return pgmname; }
    public MapField title02() { return title02; }
    public MapField curtime() { return curtime; }

    /** OPTN001..OPTN012, 1-based. */
    public MapField optn(int idx) { return optn.get(idx - 1); }
    public MapField option() { return option; }
    public MapField errmsg() { return errmsg; }
}
