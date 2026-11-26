package com.carddemo.programs.corpt00c.copybook;

import com.carddemo.programs.corpt00c.CobolText;

import java.util.List;

/**
 * CORPT00 (app/cpy-bms/CORPT00.CPY) - symbolic map storage for map CORPT0A. CORPT0AO REDEFINES CORPT0AI, so
 * the input (xxxI) and output (xxxO) data of a field share one value here. Per field: xxxL (length / cursor
 * flag, COMP S9(4)), xxxF/xxxA (input flag / attribute) and xxxC/xxxP/xxxH/xxxV (colour, PS, highlight,
 * validation) on the output side.
 */
public final class Corpt0aMap {

    /** One named field of the symbolic map. */
    public static final class Field {
        public final String name;
        public final int length;
        /** xxxL COMP S9(4). -1 requests the cursor (symbolic cursor positioning). */
        public short l;
        /** xxxF / xxxA. */
        public char f;
        /** xxxC. */
        public char c;
        /** xxxP. */
        public char p;
        /** xxxH. */
        public char h;
        /** xxxV. */
        public char v;
        private String data;

        Field(String name, int length) {
            this.name = name;
            this.length = length;
            lowValues();
        }

        /** xxxI / xxxO. */
        public String data() {
            return data;
        }

        /** MOVE value TO xxxI / xxxO (alphanumeric move; null moves LOW-VALUES). */
        public void move(String value) {
            data = CobolText.move(value, length);
        }

        /** MOVE -1 TO xxxL. */
        public void cursor() {
            l = -1;
        }

        /** INITIALIZE xxxI (alphanumeric -> SPACES). */
        public void initialize() {
            data = CobolText.spaces(length);
        }

        void lowValues() {
            l = 0;
            f = CobolText.LOW_VALUE;
            c = CobolText.LOW_VALUE;
            p = CobolText.LOW_VALUE;
            h = CobolText.LOW_VALUE;
            v = CobolText.LOW_VALUE;
            data = CobolText.lowValues(length);
        }
    }

    public final Field trnname = new Field("TRNNAME", 4);
    public final Field title01 = new Field("TITLE01", 40);
    public final Field curdate = new Field("CURDATE", 8);
    public final Field pgmname = new Field("PGMNAME", 8);
    public final Field title02 = new Field("TITLE02", 40);
    public final Field curtime = new Field("CURTIME", 8);
    public final Field monthly = new Field("MONTHLY", 1);
    public final Field yearly = new Field("YEARLY", 1);
    public final Field custom = new Field("CUSTOM", 1);
    public final Field sdtmm = new Field("SDTMM", 2);
    public final Field sdtdd = new Field("SDTDD", 2);
    public final Field sdtyyyy = new Field("SDTYYYY", 4);
    public final Field edtmm = new Field("EDTMM", 2);
    public final Field edtdd = new Field("EDTDD", 2);
    public final Field edtyyyy = new Field("EDTYYYY", 4);
    public final Field confirm = new Field("CONFIRM", 1);
    public final Field errmsg = new Field("ERRMSG", 78);

    /** Fields in map (screen) order. */
    public List<Field> fields() {
        return List.of(trnname, title01, curdate, pgmname, title02, curtime, monthly, yearly, custom,
                sdtmm, sdtdd, sdtyyyy, edtmm, edtdd, edtyyyy, confirm, errmsg);
    }

    /** MOVE LOW-VALUES TO CORPT0AO. */
    public void lowValues() {
        fields().forEach(Field::lowValues);
    }

    /**
     * EXEC CICS RECEIVE MAP('CORPT0A') INTO(CORPT0AI). The whole map is overwritten: fields the terminal did
     * not send are LOW-VALUES with length 0; sent fields are left-justified, space-padded, and xxxL holds
     * the number of characters received.
     */
    public void receive(Corpt0aInput in) {
        lowValues();
        receiveField(monthly, in.monthly());
        receiveField(yearly, in.yearly());
        receiveField(custom, in.custom());
        receiveField(sdtmm, in.sdtmm());
        receiveField(sdtdd, in.sdtdd());
        receiveField(sdtyyyy, in.sdtyyyy());
        receiveField(edtmm, in.edtmm());
        receiveField(edtdd, in.edtdd());
        receiveField(edtyyyy, in.edtyyyy());
        receiveField(confirm, in.confirm());
    }

    private static void receiveField(Field field, String value) {
        if (value == null) {
            return;
        }
        field.move(value);
        field.l = (short) Math.min(value.length(), field.length);
    }

    /** Snapshot of CORPT0AO as sent with EXEC CICS SEND MAP ... CURSOR. */
    public Corpt0aOutput toOutput() {
        String cursorField = null;
        for (Field fld : fields()) {
            if (fld.l == -1) {
                cursorField = fld.name;
                break;
            }
        }
        return new Corpt0aOutput(trnname.data(), title01.data(), curdate.data(), pgmname.data(),
                title02.data(), curtime.data(), monthly.data(), yearly.data(), custom.data(), sdtmm.data(),
                sdtdd.data(), sdtyyyy.data(), edtmm.data(), edtdd.data(), edtyyyy.data(), confirm.data(),
                errmsg.data(), errmsg.c, cursorField);
    }
}
