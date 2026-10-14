package com.carddemo.online.comen01c;

import com.carddemo.runtime.cobol.AlphanumericField;
import com.carddemo.runtime.cobol.CobolRecord;
import com.carddemo.runtime.cobol.NumericField;
import com.carddemo.runtime.cobol.PicX;
import com.carddemo.runtime.cobol.RecordLayout;
import java.util.List;
import java.util.Locale;

/**
 * COMEN02Y {@code CARDDEMO-MAIN-MENU-OPTIONS} (app/cpy/COMEN02Y.cpy): {@code CDEMO-MENU-OPT-COUNT} 9(2)
 * followed by {@code CDEMO-MENU-OPTIONS-DATA} (11 VALUE'd entries of 46 bytes), redefined as
 * {@code CDEMO-MENU-OPT OCCURS 12 TIMES}: {@code CDEMO-MENU-OPT-NUM} 9(2), {@code -NAME} X(35),
 * {@code -PGMNAME} X(8), {@code -USRTYPE} X(1).
 *
 * <p>The OCCURS 12 view is one entry longer than the VALUE data, so entry 12 is storage without a VALUE
 * (LOW-VALUES here). Subscripted access is by byte offset, like the generated code: subscript 0 or
 * 13-99 addresses storage outside the table (see {@link #usrType(int)}).
 */
public final class MenuOptions extends CobolRecord {

    public static final int ENTRY_LENGTH = 46;
    public static final int OCCURS = 12;

    private static final RecordLayout.Builder B = RecordLayout.builder("CARDDEMO-MAIN-MENU-OPTIONS");
    public static final NumericField OPT_COUNT = B.zoned("CDEMO-MENU-OPT-COUNT", 2, 0, false);
    static {
        B.group("CDEMO-MENU-OPTIONS");
        for (int i = 1; i <= OCCURS; i++) {
            B.zoned("CDEMO-MENU-OPT-NUM(" + i + ")", 2, 0, false);
            B.x("CDEMO-MENU-OPT-NAME(" + i + ")", 35);
            B.x("CDEMO-MENU-OPT-PGMNAME(" + i + ")", 8);
            B.x("CDEMO-MENU-OPT-USRTYPE(" + i + ")", 1);
        }
    }
    public static final AlphanumericField OPTIONS = B.endGroup();
    public static final RecordLayout LAYOUT = B.build();
    public static final int LENGTH = LAYOUT.length();

    /** One VALUE'd table entry. */
    public record Entry(int num, String name, String pgmName, String usrType) {
    }

    /** The COMEN02Y VALUE clauses (lines 23-89). */
    public static final List<Entry> COMEN02Y_ENTRIES = List.of(
            new Entry(1, "Account View", "COACTVWC", "U"),
            new Entry(2, "Account Update", "COACTUPC", "U"),
            new Entry(3, "Credit Card List", "COCRDLIC", "U"),
            new Entry(4, "Credit Card View", "COCRDSLC", "U"),
            new Entry(5, "Credit Card Update", "COCRDUPC", "U"),
            new Entry(6, "Transaction List", "COTRN00C", "U"),
            new Entry(7, "Transaction View", "COTRN01C", "U"),
            new Entry(8, "Transaction Add", "COTRN02C", "U"),
            new Entry(9, "Transaction Reports", "CORPT00C", "U"),
            new Entry(10, "Bill Payment", "COBIL00C", "U"),
            new Entry(11, "Pending Authorization View", "COPAUS0C", "U"));

    private MenuOptions() {
        super(LENGTH);
    }

    /** The table as compiled from COMEN02Y ({@code CDEMO-MENU-OPT-COUNT} VALUE 11). */
    public static MenuOptions comen02y() {
        return of(11, COMEN02Y_ENTRIES);
    }

    /** A table with the given count and VALUE'd entries (at most 12); remaining entries are LOW-VALUES. */
    public static MenuOptions of(int count, List<Entry> entries) {
        if (entries.size() > OCCURS) {
            throw new IllegalArgumentException("At most " + OCCURS + " entries");
        }
        MenuOptions t = new MenuOptions();
        OPT_COUNT.set(t, count);
        StringBuilder data = new StringBuilder();
        for (Entry e : entries) {
            data.append(String.format(Locale.ROOT, "%02d", e.num())).append(PicX.of(e.name(), 35)).append(PicX.of(e.pgmName(), 8))
                    .append(PicX.of(e.usrType(), 1));
        }
        String all = t.asString();
        String image = all.substring(0, OPT_COUNT.length()) + data + all.substring(OPT_COUNT.length() + data.length());
        t.setBytes(PicX.toBytes(image));
        return t;
    }

    /** CDEMO-MENU-OPT-COUNT. */
    public int count() {
        return (int) OPT_COUNT.getLong(this);
    }

    /** CDEMO-MENU-OPT-NUM(i), as its two display digits. */
    public String num(int i) {
        return bytes(i, 0, 2);
    }

    /** CDEMO-MENU-OPT-NAME(i). */
    public String name(int i) {
        return bytes(i, 2, 35);
    }

    /** CDEMO-MENU-OPT-PGMNAME(i). */
    public String pgmName(int i) {
        return bytes(i, 37, 8);
    }

    /**
     * CDEMO-MENU-OPT-USRTYPE(i). Subscript 0 addresses the byte before entry 1, i.e. the second digit of
     * CDEMO-MENU-OPT-COUNT; subscripts past the record address storage outside this 01 item, assumed
     * LOW-VALUES.
     */
    public String usrType(int i) {
        return bytes(i, 45, 1);
    }

    private String bytes(int subscript, int offsetInEntry, int length) {
        int start = OPT_COUNT.length() + (subscript - 1) * ENTRY_LENGTH + offsetInEntry;
        String image = asString();
        StringBuilder out = new StringBuilder(length);
        for (int p = start; p < start + length; p++) {
            out.append(p >= 0 && p < image.length() ? image.charAt(p) : PicX.LOW_VALUE);
        }
        return out.toString();
    }
}
