package com.carddemo.online.coactupc;

import com.carddemo.common.copybook.CardDemoCommarea;
import com.carddemo.runtime.cobol.AlphanumericField;
import com.carddemo.runtime.cobol.CobolField;
import com.carddemo.runtime.cobol.CobolRecord;
import com.carddemo.runtime.cobol.NumericField;
import com.carddemo.runtime.cobol.RecordLayout;
import java.math.BigDecimal;

/**
 * COACTUPC program-private record layouts (app/cbl/COACTUPC.cbl), field names = COBOL names:
 * <ul>
 *   <li>{@link #PROG_COMMAREA} - {@code WS-THIS-PROGCOMMAREA} (lines 652-858): ACUP-CHANGE-ACTION,
 *       ACUP-OLD-DETAILS, ACUP-NEW-DETAILS (873 bytes) with their REDEFINES;</li>
 *   <li>{@link #WS_COMMAREA} - {@code WS-COMMAREA} X(2000) as returned by COMMON-RETURN: CARDDEMO-COMMAREA
 *       (160) + WS-THIS-PROGCOMMAREA + spaces (used to decode the REST response);</li>
 *   <li>{@link #ACCT_UPDATE} - {@code ACCT-UPDATE-RECORD} (lines 418-433, 300 bytes; note: no ADDR-ZIP);</li>
 *   <li>{@link #CUST_UPDATE} - {@code CUST-UPDATE-RECORD} (lines 434-456, 500 bytes).</li>
 * </ul>
 */
final class AcupRecords {

    static final RecordLayout PROG_COMMAREA;
    static final RecordLayout WS_COMMAREA;
    static final RecordLayout ACCT_UPDATE;
    static final RecordLayout CUST_UPDATE;
    static final int WS_COMMAREA_LENGTH = 2000;

    static {
        RecordLayout.Builder p = RecordLayout.builder("WS-THIS-PROGCOMMAREA");
        addProgCommarea(p);
        PROG_COMMAREA = p.build();

        RecordLayout.Builder w = RecordLayout.builder("WS-COMMAREA");
        for (CobolField f : CardDemoCommarea.LAYOUT.fields()) {
            if (f instanceof NumericField n) {
                w.zoned(n.name(), n.integerDigits(), n.scale(), n.signed());
            } else {
                w.x(f.name(), f.length());
            }
        }
        addProgCommarea(w);
        w.filler(WS_COMMAREA_LENGTH - CardDemoCommarea.LENGTH - PROG_COMMAREA.length());
        WS_COMMAREA = w.build();

        RecordLayout.Builder a = RecordLayout.builder("ACCT-UPDATE-RECORD");
        a.zoned("ACCT-UPDATE-ID", 11, 0, false);
        a.x("ACCT-UPDATE-ACTIVE-STATUS", 1);
        a.zoned("ACCT-UPDATE-CURR-BAL", 10, 2, true);
        a.zoned("ACCT-UPDATE-CREDIT-LIMIT", 10, 2, true);
        a.zoned("ACCT-UPDATE-CASH-CREDIT-LIMIT", 10, 2, true);
        a.x("ACCT-UPDATE-OPEN-DATE", 10);
        a.x("ACCT-UPDATE-EXPIRAION-DATE", 10);
        a.x("ACCT-UPDATE-REISSUE-DATE", 10);
        a.zoned("ACCT-UPDATE-CURR-CYC-CREDIT", 10, 2, true);
        a.zoned("ACCT-UPDATE-CURR-CYC-DEBIT", 10, 2, true);
        a.x("ACCT-UPDATE-GROUP-ID", 10);
        a.filler(188);
        ACCT_UPDATE = a.build();

        RecordLayout.Builder c = RecordLayout.builder("CUST-UPDATE-RECORD");
        c.zoned("CUST-UPDATE-ID", 9, 0, false);
        c.x("CUST-UPDATE-FIRST-NAME", 25);
        c.x("CUST-UPDATE-MIDDLE-NAME", 25);
        c.x("CUST-UPDATE-LAST-NAME", 25);
        c.x("CUST-UPDATE-ADDR-LINE-1", 50);
        c.x("CUST-UPDATE-ADDR-LINE-2", 50);
        c.x("CUST-UPDATE-ADDR-LINE-3", 50);
        c.x("CUST-UPDATE-ADDR-STATE-CD", 2);
        c.x("CUST-UPDATE-ADDR-COUNTRY-CD", 3);
        c.x("CUST-UPDATE-ADDR-ZIP", 10);
        c.x("CUST-UPDATE-PHONE-NUM-1", 15);
        c.x("CUST-UPDATE-PHONE-NUM-2", 15);
        c.zoned("CUST-UPDATE-SSN", 9, 0, false);
        c.x("CUST-UPDATE-GOVT-ISSUED-ID", 20);
        c.x("CUST-UPDATE-DOB-YYYY-MM-DD", 10);
        c.x("CUST-UPDATE-EFT-ACCOUNT-ID", 10);
        c.x("CUST-UPDATE-PRI-CARD-IND", 1);
        c.zoned("CUST-UPDATE-FICO-CREDIT-SCORE", 3, 0, false);
        c.filler(168);
        CUST_UPDATE = c.build();
    }

    private AcupRecords() {
    }

    private static void addProgCommarea(RecordLayout.Builder b) {
        b.x("ACUP-CHANGE-ACTION", 1);
        addDetails(b, "ACUP-OLD-");
        addDetails(b, "ACUP-NEW-");
    }

    private static void addDetails(RecordLayout.Builder b, String p) {
        boolean isNew = p.equals("ACUP-NEW-");
        b.group(p + "DETAILS");
        b.group(p + "ACCT-DATA");
        redefNum(b, b.x(p + "ACCT-ID-X", 11), p + "ACCT-ID", 11, 0, false);
        b.x(p + "ACTIVE-STATUS", 1);
        redefNum(b, b.x(p + "CURR-BAL", 12), p + "CURR-BAL-N", 10, 2, true);
        redefNum(b, b.x(p + "CREDIT-LIMIT", 12), p + "CREDIT-LIMIT-N", 10, 2, true);
        redefNum(b, b.x(p + "CASH-CREDIT-LIMIT", 12), p + "CASH-CREDIT-LIMIT-N", 10, 2, true);
        redefDate(b, b.x(p + "OPEN-DATE", 8), p + "OPEN-YEAR", p + "OPEN-MON", p + "OPEN-DAY");
        redefDate(b, b.x(p + "EXPIRAION-DATE", 8), p + "EXP-YEAR", p + "EXP-MON", p + "EXP-DAY");
        redefDate(b, b.x(p + "REISSUE-DATE", 8), p + "REISSUE-YEAR", p + "REISSUE-MON", p + "REISSUE-DAY");
        redefNum(b, b.x(p + "CURR-CYC-CREDIT", 12), p + "CURR-CYC-CREDIT-N", 10, 2, true);
        redefNum(b, b.x(p + "CURR-CYC-DEBIT", 12), p + "CURR-CYC-DEBIT-N", 10, 2, true);
        b.x(p + "GROUP-ID", 10);
        b.endGroup();
        b.group(p + "CUST-DATA");
        redefNum(b, b.x(p + "CUST-ID-X", 9), p + "CUST-ID", 9, 0, false);
        b.x(p + "CUST-FIRST-NAME", 25);
        b.x(p + "CUST-MIDDLE-NAME", 25);
        b.x(p + "CUST-LAST-NAME", 25);
        b.x(p + "CUST-ADDR-LINE-1", 50);
        b.x(p + "CUST-ADDR-LINE-2", 50);
        b.x(p + "CUST-ADDR-LINE-3", 50);
        b.x(p + "CUST-ADDR-STATE-CD", 2);
        b.x(p + "CUST-ADDR-COUNTRY-CD", 3);
        b.x(p + "CUST-ADDR-ZIP", 10);
        redefPhone(b, b.x(p + "CUST-PHONE-NUM-1", 15), p + "CUST-PHONE-NUM-1");
        redefPhone(b, b.x(p + "CUST-PHONE-NUM-2", 15), p + "CUST-PHONE-NUM-2");
        AlphanumericField ssn;
        if (isNew) {
            b.group(p + "CUST-SSN-X");
            b.x(p + "CUST-SSN-1", 3);
            b.x(p + "CUST-SSN-2", 2);
            b.x(p + "CUST-SSN-3", 4);
            ssn = b.endGroup();
        } else {
            ssn = b.x(p + "CUST-SSN-X", 9);
        }
        redefNum(b, ssn, p + "CUST-SSN", 9, 0, false);
        b.x(p + "CUST-GOVT-ISSUED-ID", 20);
        redefDate(b, b.x(p + "CUST-DOB-YYYY-MM-DD", 8), p + "CUST-DOB-YEAR", p + "CUST-DOB-MON", p + "CUST-DOB-DAY");
        b.x(p + "CUST-EFT-ACCOUNT-ID", 10);
        b.x(p + "CUST-PRI-HOLDER-IND", 1);
        redefNum(b, b.x(p + "CUST-FICO-SCORE-X", 3), p + "CUST-FICO-SCORE", 3, 0, false);
        b.endGroup();
        b.endGroup();
    }

    private static void redefNum(RecordLayout.Builder b, CobolField target, String name, int digits, int scale,
                                 boolean signed) {
        b.redefines(target);
        b.zoned(name, digits, scale, signed);
        b.endRedefines(target);
    }

    private static void redefDate(RecordLayout.Builder b, CobolField target, String year, String mon, String day) {
        b.redefines(target);
        b.x(year, 4);
        b.x(mon, 2);
        b.x(day, 2);
        b.endRedefines(target);
    }

    private static void redefPhone(RecordLayout.Builder b, CobolField target, String p) {
        b.redefines(target);
        b.filler(1);
        b.x(p + "A", 3);
        b.filler(1);
        b.x(p + "B", 3);
        b.filler(1);
        b.x(p + "C", 4);
        b.filler(2);
        b.endRedefines(target);
    }

    /** A record over {@code layout} with name-based access (names are the COBOL data names). */
    static final class Rec extends CobolRecord {
        private final RecordLayout layout;

        Rec(RecordLayout layout) {
            super(layout.length());
            this.layout = layout;
        }

        RecordLayout layout() {
            return layout;
        }

        String x(String name) {
            return layout.field(name) instanceof NumericField n ? n.raw(this) : ((AlphanumericField) layout.field(name)).get(this);
        }

        void setX(String name, String value) {
            CobolField f = layout.field(name);
            if (f instanceof AlphanumericField a) {
                a.set(this, value);
            } else {
                // MOVE alphanumeric TO numeric DISPLAY of the same length: bytes copied unchanged
                new AlphanumericField(f.name(), f.offset(), f.length()).set(this, value);
            }
        }

        /** Numeric value of a DISPLAY item, IBM semantics for invalid data (see {@link DisplayNumerics}). */
        BigDecimal num(String name) {
            NumericField n = (NumericField) layout.field(name);
            return DisplayNumerics.value(n.raw(this), n.scale(), n.signed());
        }

        /** MOVE / COMPUTE numeric value TO item (truncating). */
        void setNum(String name, BigDecimal value) {
            ((NumericField) layout.field(name)).set(this, value);
        }

        void fill(String name, char c) {
            layout.field(name).fill(this, c);
        }

        /** INITIALIZE: alphanumeric elementary items to SPACES, numeric to ZERO; FILLER untouched. */
        void initializeItems() {
            for (CobolField f : layout.fields()) {
                if (f instanceof NumericField n) {
                    n.set(this, BigDecimal.ZERO);
                } else {
                    f.fill(this, ' ');
                }
            }
        }
    }
}
