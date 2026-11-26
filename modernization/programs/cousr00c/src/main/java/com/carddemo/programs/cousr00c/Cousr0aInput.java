package com.carddemo.programs.cousr00c;

import com.carddemo.programs.cousr00c.copybook.Cousr00Map;
import java.util.ArrayList;
import java.util.List;

/**
 * Request DTO: the COUSR0A fields as returned by the terminal for RECEIVE MAP. All fields are
 * FSET in the BMS source, so the terminal echoes every field; {@code null} means "not transmitted".
 */
public record Cousr0aInput(
        String trnname,
        String title01,
        String curdate,
        String pgmname,
        String title02,
        String curtime,
        String pagenum,
        String usridin,
        List<Cousr0aScreen.Row> rows,
        String errmsg) {

    public Cousr0aInput {
        List<Cousr0aScreen.Row> r = new ArrayList<>(rows == null ? List.of() : rows);
        while (r.size() < Cousr00Map.ROWS) {
            r.add(new Cousr0aScreen.Row(null, null, null, null, null));
        }
        rows = java.util.Collections.unmodifiableList(r);
    }

    /** Nothing transmitted. */
    public static Cousr0aInput empty() {
        return new Cousr0aInput(null, null, null, null, null, null, null, null, List.of(), null);
    }

    /** What the terminal sends back after displaying {@code screen} unchanged (low-values are not echoed). */
    public static Cousr0aInput echo(Cousr0aScreen screen) {
        List<Cousr0aScreen.Row> rows = new ArrayList<>();
        for (Cousr0aScreen.Row row : screen.rows()) {
            rows.add(new Cousr0aScreen.Row(shown(row.sel()), shown(row.usrid()), shown(row.fname()),
                    shown(row.lname()), shown(row.utype())));
        }
        return new Cousr0aInput(shown(screen.trnname()), shown(screen.title01()), shown(screen.curdate()),
                shown(screen.pgmname()), shown(screen.title02()), shown(screen.curtime()), shown(screen.pagenum()),
                shown(screen.usridin()), rows, shown(screen.errmsg()));
    }

    private static String shown(String v) {
        return v == null || Cobol.isAll(v, Cobol.LOW_VALUE) ? null : v;
    }

    /** User typed a search key in "Search User ID". */
    public Cousr0aInput withUsridin(String value) {
        return new Cousr0aInput(trnname, title01, curdate, pgmname, title02, curtime, pagenum, value, rows, errmsg);
    }

    /** User typed a selection code on list line {@code row} (1-based). */
    public Cousr0aInput withSelection(int row, String value) {
        List<Cousr0aScreen.Row> r = new ArrayList<>(rows);
        Cousr0aScreen.Row old = r.get(row - 1);
        r.set(row - 1, new Cousr0aScreen.Row(value, old.usrid(), old.fname(), old.lname(), old.utype()));
        return new Cousr0aInput(trnname, title01, curdate, pgmname, title02, curtime, pagenum, usridin, r, errmsg);
    }
}
