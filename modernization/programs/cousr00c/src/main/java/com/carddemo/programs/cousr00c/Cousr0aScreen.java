package com.carddemo.programs.cousr00c;

import com.carddemo.programs.cousr00c.copybook.Cousr00Map;
import java.util.ArrayList;
import java.util.List;

/**
 * Response DTO: the COUSR0A screen exactly as sent by one SEND MAP (raw field contents,
 * low-values included). {@code cursorField} is the symbolic-cursor target.
 */
public record Cousr0aScreen(
        String trnname,
        String title01,
        String curdate,
        String pgmname,
        String title02,
        String curtime,
        String pagenum,
        String usridin,
        List<Row> rows,
        String errmsg,
        String cursorField) {

    /** One list line: SELnnnn, USRIDnn, FNAMEnn, LNAMEnn, UTYPEnn. */
    public record Row(String sel, String usrid, String fname, String lname, String utype) {
    }

    public Cousr0aScreen {
        rows = List.copyOf(rows);
    }

    static Cousr0aScreen of(Cousr00Map map) {
        List<Row> rows = new ArrayList<>();
        for (int i = 0; i < Cousr00Map.ROWS; i++) {
            rows.add(new Row(map.sel[i].data(), map.usrid[i].data(), map.fname[i].data(), map.lname[i].data(),
                    map.utype[i].data()));
        }
        return new Cousr0aScreen(map.trnname.data(), map.title01.data(), map.curdate.data(), map.pgmname.data(),
                map.title02.data(), map.curtime.data(), map.pagenum.data(), map.usridin.data(), rows,
                map.errmsg.data(), map.cursorField());
    }
}
