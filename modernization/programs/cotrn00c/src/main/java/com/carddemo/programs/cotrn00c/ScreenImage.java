package com.carddemo.programs.cotrn00c;

import com.carddemo.programs.cotrn00c.copybook.Cotrn00Map;

import java.util.ArrayList;
import java.util.List;

/** Snapshot of COTRN0AO at one EXEC CICS SEND MAP('COTRN0A') MAPSET('COTRN00'). Values are raw fixed-width. */
public record ScreenImage(
        String trnname,
        String title01,
        String curdate,
        String pgmname,
        String title02,
        String curtime,
        String pagenum,
        String trnidin,
        List<Row> rows,
        String errmsg,
        String cursorField,
        boolean erase) {

    public record Row(String sel, String trnid, String tdate, String tdesc, String tamt) {
    }

    static ScreenImage of(Cotrn00Map map, boolean erase) {
        List<Row> rows = new ArrayList<>();
        for (int i = 1; i <= Cotrn00Map.ROWS; i++) {
            Cotrn00Map.Row r = map.row(i);
            rows.add(new Row(r.sel.data(), r.trnid.data(), r.tdate.data(), r.tdesc.data(), r.tamt.data()));
        }
        return new ScreenImage(map.trnname.data(), map.title01.data(), map.curdate.data(), map.pgmname.data(),
                map.title02.data(), map.curtime.data(), map.pagenum.data(), map.trnidin.data(), List.copyOf(rows),
                map.errmsg.data(), map.cursorField(), erase);
    }

    /** Row 1..10. */
    public Row row(int n) {
        return rows.get(n - 1);
    }
}
