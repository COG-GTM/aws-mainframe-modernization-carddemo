package com.carddemo.programs.copaus0c.copybook;

import com.carddemo.programs.copaus0c.cobol.Cobol;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * COPAU00 symbolic map storage: COPAU0AI with COPAU0AO REDEFINES COPAU0AI. Every field has its
 * length (L), attribute (A/F) and data (I = O) slot, sharing storage exactly as the BMS copybook does.
 */
public final class Copau0aMap {

    /** One BMS field: {@code xxxL PIC S9(4) COMP}, {@code xxxA/F PIC X}, {@code xxxI/O PIC X(n)}. */
    public static final class Field {
        public final String name;
        public final int size;
        public int length;
        public char attr;
        private String data;

        Field(String name, int size) {
            this.name = name;
            this.size = size;
            this.data = Cobol.lowValues(size);
        }

        public String get() {
            return data;
        }

        /** MOVE source TO field (alphanumeric move: pad/truncate). */
        public void move(String source) {
            data = Cobol.moveX(source, size);
        }

        /** Raw store of an already-sized image (used by STRING ... INTO). */
        public void setRaw(String image) {
            data = Cobol.moveX(image, size);
        }

        void lowValues() {
            data = Cobol.lowValues(size);
            length = 0;
            attr = Cobol.LOW_VALUE;
        }
    }

    /** One of the five authorization list rows. */
    public static final class Row {
        public final Field sel;
        public final Field trnid;
        public final Field pdate;
        public final Field ptime;
        public final Field ptype;
        public final Field paprv;
        public final Field pstat;
        public final Field pamt;

        Row(int n, Copau0aMap map) {
            sel = map.add("SEL000" + n, 1);
            trnid = map.add("TRNID0" + n, 16);
            pdate = map.add("PDATE0" + n, 8);
            ptime = map.add("PTIME0" + n, 8);
            ptype = map.add("PTYPE0" + n, 4);
            paprv = map.add("PAPRV0" + n, 1);
            pstat = map.add("PSTAT0" + n, 1);
            pamt = map.add("PAMT00" + n, 12);
        }
    }

    private final Map<String, Field> fields = new LinkedHashMap<>();

    public final Field trnname = add("TRNNAME", 4);
    public final Field title01 = add("TITLE01", 40);
    public final Field curdate = add("CURDATE", 8);
    public final Field pgmname = add("PGMNAME", 8);
    public final Field title02 = add("TITLE02", 40);
    public final Field curtime = add("CURTIME", 8);
    public final Field acctid = add("ACCTID", 11);
    public final Field cname = add("CNAME", 25);
    public final Field custid = add("CUSTID", 9);
    public final Field addr001 = add("ADDR001", 25);
    public final Field accstat = add("ACCSTAT", 1);
    public final Field addr002 = add("ADDR002", 25);
    public final Field phone1 = add("PHONE1", 13);
    public final Field apprcnt = add("APPRCNT", 3);
    public final Field declcnt = add("DECLCNT", 3);
    public final Field credlim = add("CREDLIM", 12);
    public final Field cashlim = add("CASHLIM", 9);
    public final Field appramt = add("APPRAMT", 10);
    public final Field credbal = add("CREDBAL", 12);
    public final Field cashbal = add("CASHBAL", 9);
    public final Field declamt = add("DECLAMT", 10);
    public final List<Row> rows = createRows();
    public final Field errmsg = add("ERRMSG", 78);

    private List<Row> createRows() {
        List<Row> list = new ArrayList<>();
        for (int i = 1; i <= 5; i++) {
            list.add(new Row(i, this));
        }
        return List.copyOf(list);
    }

    private Field add(String name, int size) {
        Field f = new Field(name, size);
        fields.put(name, f);
        return f;
    }

    /** 1-based row access (SEL0001 .. SEL0005). */
    public Row row(int n) {
        return rows.get(n - 1);
    }

    /** MOVE LOW-VALUES TO COPAU0AO. */
    public void lowValues() {
        fields.values().forEach(Field::lowValues);
    }

    /** First field whose length is -1, i.e. where SEND ... CURSOR places the cursor. */
    public String cursorField() {
        return fields.values().stream().filter(f -> f.length == -1).map(f -> f.name).findFirst().orElse(null);
    }

    /**
     * EXEC CICS RECEIVE MAP INTO(COPAU0AI): modified fields carry the terminal data, every other field
     * is set to nulls. Only the unprotected fields (ACCTID, SEL0001-5) can be modified by the operator.
     */
    public void receive(Copau0aInput input) {
        lowValues();
        input.fsetEcho().forEach((name, value) -> receiveField(fields.get(name), value));
        receiveField(acctid, input.acctId());
        for (int i = 1; i <= 5; i++) {
            receiveField(row(i).sel, input.selection(i));
        }
    }

    private static void receiveField(Field f, String value) {
        if (value != null) {
            f.move(value);
            f.length = Math.min(value.length(), f.size);
        }
    }

    /** Snapshot of the COPAU0AO image for EXEC CICS SEND MAP. */
    public Copau0aScreen snapshot(boolean erase) {
        List<Copau0aScreen.Row> out = new ArrayList<>();
        for (Row r : rows) {
            out.add(new Copau0aScreen.Row(r.sel.get(), r.sel.attr, r.trnid.get(), r.pdate.get(), r.ptime.get(),
                    r.ptype.get(), r.paprv.get(), r.pstat.get(), r.pamt.get()));
        }
        return new Copau0aScreen(trnname.get(), title01.get(), curdate.get(), pgmname.get(), title02.get(),
                curtime.get(), acctid.get(), cname.get(), custid.get(), addr001.get(), accstat.get(), addr002.get(),
                phone1.get(), apprcnt.get(), declcnt.get(), credlim.get(), cashlim.get(), appramt.get(),
                credbal.get(), cashbal.get(), declamt.get(), out, errmsg.get(), erase, cursorField());
    }
}
