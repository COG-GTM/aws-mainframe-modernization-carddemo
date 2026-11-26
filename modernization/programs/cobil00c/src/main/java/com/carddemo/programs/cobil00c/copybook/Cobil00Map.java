package com.carddemo.programs.cobil00c.copybook;

import java.util.List;

/**
 * COBIL00 symbolic map (cpy-bms/COBIL00.CPY): {@code COBIL0AI} with {@code COBIL0AO REDEFINES
 * COBIL0AI}. Because the output map redefines the input map, each field's I and O data share
 * storage; {@link BmsField} models that single storage area plus its L/F(A)/C/P/H/V bytes.
 */
public final class Cobil00Map {

    /** One named DFHMDF field: {@code xxxL COMP S9(4)}, {@code xxxF/xxxA}, {@code xxxC/P/H/V}, {@code xxxI/xxxO}. */
    public static final class BmsField {
        private final String name;
        private final int dataLength;
        private short length;      // xxxL  (MOVE -1 = symbolic cursor positioning)
        private char attribute;    // xxxF / xxxA
        private char color;        // xxxC
        private char ps;           // xxxP
        private char hilight;      // xxxH
        private char validn;       // xxxV
        private String data;       // xxxI / xxxO

        BmsField(String name, int dataLength) {
            this.name = name;
            this.dataLength = dataLength;
            lowValues();
        }

        void lowValues() {
            length = 0;
            attribute = Cobol.LOW_VALUE;
            color = Cobol.LOW_VALUE;
            ps = Cobol.LOW_VALUE;
            hilight = Cobol.LOW_VALUE;
            validn = Cobol.LOW_VALUE;
            data = Cobol.lowValues(dataLength);
        }

        /** RECEIVE MAP INTO: field not received is X'00' with length 0; attribute bytes cleared. */
        public void lowValuesForReceive() {
            lowValues();
        }

        public String name() { return name; }
        public int dataLength() { return dataLength; }
        public short length() { return length; }
        public void length(int l) { length = (short) l; }
        public char attribute() { return attribute; }
        public void attribute(char a) { attribute = a; }
        public char color() { return color; }
        public void color(char c) { color = c; }
        public char ps() { return ps; }
        public char hilight() { return hilight; }
        public char validn() { return validn; }
        public String data() { return data; }
        /** MOVE alphanumeric into xxxI / xxxO (PIC X(n)). */
        public void data(String value) { data = Cobol.alnum(value, dataLength); }
        /** MOVE SPACES. */
        public void spaces() { data = Cobol.spaces(dataLength); }
    }

    public final BmsField trnname = new BmsField("TRNNAME", 4);
    public final BmsField title01 = new BmsField("TITLE01", 40);
    public final BmsField curdate = new BmsField("CURDATE", 8);
    public final BmsField pgmname = new BmsField("PGMNAME", 8);
    public final BmsField title02 = new BmsField("TITLE02", 40);
    public final BmsField curtime = new BmsField("CURTIME", 8);
    public final BmsField actidin = new BmsField("ACTIDIN", 11);
    public final BmsField curbal = new BmsField("CURBAL", 14);
    public final BmsField confirm = new BmsField("CONFIRM", 1);
    public final BmsField errmsg = new BmsField("ERRMSG", 78);

    /** Fields in map (screen) order. */
    public final List<BmsField> fields = List.of(trnname, title01, curdate, pgmname, title02, curtime,
            actidin, curbal, confirm, errmsg);

    /** Field defined with the IC attribute in COBIL00.bms (initial cursor). */
    public static final String INITIAL_CURSOR_FIELD = "ACTIDIN";

    /** {@code MOVE LOW-VALUES TO COBIL0AO}. */
    public void lowValues() {
        fields.forEach(BmsField::lowValues);
    }

    /**
     * SEND MAP ... CURSOR (symbolic cursor positioning): the cursor goes to the first field whose
     * length subfield is -1; with none, the IC field of the physical map.
     */
    public String cursorField() {
        return fields.stream().filter(f -> f.length() == -1).map(BmsField::name).findFirst()
                .orElse(INITIAL_CURSOR_FIELD);
    }
}
