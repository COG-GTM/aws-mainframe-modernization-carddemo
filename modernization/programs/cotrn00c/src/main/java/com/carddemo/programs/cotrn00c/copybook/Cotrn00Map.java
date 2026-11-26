package com.carddemo.programs.cotrn00c.copybook;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/** COTRN00 (app/cpy-bms/COTRN00.CPY) - symbolic map COTRN0AI / COTRN0AO of mapset COTRN00. */
public final class Cotrn00Map {

    public static final String MAP = "COTRN0A";
    public static final String MAPSET = "COTRN00";
    public static final int ROWS = 10;

    /** One list row: SELnnnn, TRNIDnn, TDATEnn, TDESCnn, TAMTnnn. */
    public static final class Row {
        public final MapField sel;
        public final MapField trnid;
        public final MapField tdate;
        public final MapField tdesc;
        public final MapField tamt;

        Row(int n) {
            sel = new MapField(String.format("SEL%04d", n), 1);
            trnid = new MapField(String.format("TRNID%02d", n), 16);
            tdate = new MapField(String.format("TDATE%02d", n), 8);
            tdesc = new MapField(String.format("TDESC%02d", n), 26);
            tamt = new MapField(String.format("TAMT%03d", n), 12);
        }
    }

    public final MapField trnname = new MapField("TRNNAME", 4);
    public final MapField title01 = new MapField("TITLE01", 40);
    public final MapField curdate = new MapField("CURDATE", 8);
    public final MapField pgmname = new MapField("PGMNAME", 8);
    public final MapField title02 = new MapField("TITLE02", 40);
    public final MapField curtime = new MapField("CURTIME", 8);
    public final MapField pagenum = new MapField("PAGENUM", 8);
    public final MapField trnidin = new MapField("TRNIDIN", 16);
    private final List<Row> rows;
    public final MapField errmsg = new MapField("ERRMSG", 78);

    public Cotrn00Map() {
        List<Row> r = new ArrayList<>();
        for (int i = 1; i <= ROWS; i++) {
            r.add(new Row(i));
        }
        rows = Collections.unmodifiableList(r);
    }

    /** Row 1..10. */
    public Row row(int n) {
        return rows.get(n - 1);
    }

    /** Fields in map (storage) order. */
    public List<MapField> fields() {
        List<MapField> all = new ArrayList<>(List.of(trnname, title01, curdate, pgmname, title02, curtime,
                pagenum, trnidin));
        for (Row r : rows) {
            all.addAll(List.of(r.sel, r.trnid, r.tdate, r.tdesc, r.tamt));
        }
        all.add(errmsg);
        return all;
    }

    /** MOVE LOW-VALUES TO COTRN0AO. */
    public void setLowValues() {
        fields().forEach(MapField::setLowValues);
    }

    /** Symbolic cursor (SEND ... CURSOR): the first field whose length is -1, or null. */
    public String cursorField() {
        return fields().stream().filter(f -> f.length() == -1).map(MapField::name).findFirst().orElse(null);
    }
}
