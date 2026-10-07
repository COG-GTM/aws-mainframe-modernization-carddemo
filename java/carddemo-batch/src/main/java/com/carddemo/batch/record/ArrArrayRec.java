package com.carddemo.batch.record;

import com.carddemo.batch.codec.Field;
import com.carddemo.batch.codec.FixedWidth;
import com.carddemo.batch.codec.Layout;

import java.math.BigDecimal;

/**
 * {@code FD ARRY-FILE / 01 ARR-ARRAY-REC} of CBACT01C (110 bytes).
 * <pre>
 * 05  ARR-ACCT-ID                PIC 9(11).
 * 05  ARR-ACCT-BAL OCCURS 5 TIMES.
 *   10  ARR-ACCT-CURR-BAL        PIC S9(10)V99.
 *   10  ARR-ACCT-CURR-CYC-DEBIT  PIC S9(10)V99 COMP-3.
 * 05  ARR-FILLER                 PIC X(04).
 * </pre>
 * The OCCURS table is exposed as the fixed-size array {@link #arrAcctBal()} of five {@link ArrAcctBal} views.
 */
public final class ArrArrayRec extends FixedWidthRecord {

    public static final int OCCURS = 5;

    private static final Layout.Builder B = Layout.builder("ARR-ARRAY-REC");
    public static final Field ARR_ACCT_ID = B.unsigned("ARR-ACCT-ID", 11);
    public static final Field ARR_ACCT_BAL = B.group("ARR-ACCT-BAL", OCCURS, g -> {
        g.zoned("ARR-ACCT-CURR-BAL", 10, 2);
        g.packed("ARR-ACCT-CURR-CYC-DEBIT", 10, 2);
    });
    public static final Field ARR_FILLER = B.text("ARR-FILLER", 4);
    public static final Layout LAYOUT = B.build();
    public static final int LENGTH = LAYOUT.length();

    private final ArrAcctBal[] arrAcctBal = new ArrAcctBal[OCCURS];

    public ArrArrayRec() {
        super(LAYOUT);
        bindOccurrences();
    }

    private ArrArrayRec(byte[] raw) {
        super(LAYOUT, raw);
        bindOccurrences();
    }

    private void bindOccurrences() {
        for (int i = 0; i < OCCURS; i++) {
            arrAcctBal[i] = new ArrAcctBal(ARR_ACCT_BAL.occurrence(i));
        }
    }

    public static ArrArrayRec decode(byte[] raw) {
        return new ArrArrayRec(raw);
    }

    @Override
    public Layout layout() {
        return LAYOUT;
    }

    public long arrAcctId() {
        return FixedWidth.unsigned(data, ARR_ACCT_ID);
    }

    public void setArrAcctId(long v) {
        FixedWidth.setUnsigned(data, ARR_ACCT_ID, v);
    }

    /** The five occurrences of ARR-ACCT-BAL, index 0 = COBOL occurrence 1. */
    public ArrAcctBal[] arrAcctBal() {
        return arrAcctBal;
    }

    /** COBOL style 1-based subscript: {@code ARR-ACCT-BAL(n)}. */
    public ArrAcctBal arrAcctBal(int occurrence) {
        return arrAcctBal[occurrence - 1];
    }

    public String arrFiller() {
        return FixedWidth.text(data, ARR_FILLER);
    }

    /** One occurrence of the {@code ARR-ACCT-BAL} group, a view over the record buffer. */
    public final class ArrAcctBal {
        private final Field currBal;
        private final Field currCycDebit;

        private ArrAcctBal(Field occurrence) {
            this.currBal = occurrence.child("ARR-ACCT-CURR-BAL");
            this.currCycDebit = occurrence.child("ARR-ACCT-CURR-CYC-DEBIT");
        }

        public BigDecimal arrAcctCurrBal() {
            return FixedWidth.decimal(data, currBal);
        }

        public void setArrAcctCurrBal(BigDecimal v) {
            FixedWidth.setDecimal(data, currBal, v);
        }

        /** COMP-3 field. */
        public BigDecimal arrAcctCurrCycDebit() {
            return FixedWidth.decimal(data, currCycDebit);
        }

        public void setArrAcctCurrCycDebit(BigDecimal v) {
            FixedWidth.setDecimal(data, currCycDebit, v);
        }
    }
}
