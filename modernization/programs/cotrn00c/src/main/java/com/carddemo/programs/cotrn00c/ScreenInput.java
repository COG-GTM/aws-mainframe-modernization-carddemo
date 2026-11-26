package com.carddemo.programs.cotrn00c;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Data received by EXEC CICS RECEIVE MAP('COTRN0A') INTO(COTRN0AI). A {@code null} field was not
 * transmitted (reads as LOW-VALUES); a non-null value is left-justified and space padded
 * (BMS default JUSTIFY=(LEFT,BLANK)). All COTRN0A fields are FSET, so the rows sent on the previous
 * screen come back here, which is how a selection maps to a TRAN-ID.
 */
public record ScreenInput(String trnidin, String pagenum, List<RowInput> rows) {

    public record RowInput(String sel, String trnid, String tdate, String tdesc, String tamt) {

        public static RowInput empty() {
            return new RowInput(null, null, null, null, null);
        }
    }

    public ScreenInput {
        List<RowInput> r = new ArrayList<>(rows == null ? List.of() : rows);
        while (r.size() < 10) {
            r.add(RowInput.empty());
        }
        if (r.size() > 10) {
            throw new IllegalArgumentException("COTRN0A has 10 rows");
        }
        rows = List.copyOf(r);
    }

    public static ScreenInput empty() {
        return new ScreenInput(null, null, List.of());
    }

    /** The screen exactly as last sent (FSET round trip), with optional overrides applied by the caller. */
    public static ScreenInput echo(ScreenImage screen) {
        List<RowInput> rows = new ArrayList<>();
        for (ScreenImage.Row r : screen.rows()) {
            rows.add(new RowInput(r.sel(), r.trnid(), r.tdate(), r.tdesc(), r.tamt()));
        }
        return new ScreenInput(screen.trnidin(), screen.pagenum(), rows);
    }

    public ScreenInput withTrnidin(String value) {
        return new ScreenInput(value, pagenum, rows);
    }

    /** Sets the selection code on row 1..10. */
    public ScreenInput withSelection(int row, String sel) {
        RowInput[] r = rows.toArray(new RowInput[0]);
        RowInput old = r[row - 1];
        r[row - 1] = new RowInput(sel, old.trnid(), old.tdate(), old.tdesc(), old.tamt());
        return new ScreenInput(trnidin, pagenum, Arrays.asList(r));
    }
}
