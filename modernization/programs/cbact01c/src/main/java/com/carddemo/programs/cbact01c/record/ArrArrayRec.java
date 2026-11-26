package com.carddemo.programs.cbact01c.record;

import com.carddemo.programs.cbact01c.support.FieldDef;
import com.carddemo.programs.cbact01c.support.RecordBuffer;

/**
 * {@code FD ARRY-FILE / 01 ARR-ARRAY-REC} (DD ARRYFILE, LRECL=110 RECFM=FB):
 * ARR-ACCT-ID, ARR-ACCT-BAL OCCURS 5 TIMES (ARR-ACCT-CURR-BAL S9(10)V99, ARR-ACCT-CURR-CYC-DEBIT S9(10)V99 COMP-3),
 * ARR-FILLER X(04).
 */
public final class ArrArrayRec extends RecordBuffer {
    public static final int LENGTH = 110;
    public static final int OCCURS = 5;
    private static final int ENTRY_LENGTH = 12 + 7;

    public static final FieldDef ARR_ACCT_ID = FieldDef.n("ARR-ACCT-ID", 0, 11);
    public static final FieldDef ARR_FILLER = FieldDef.x("ARR-FILLER", 11 + OCCURS * ENTRY_LENGTH, 4);

    public ArrArrayRec() {
        super(LENGTH, (byte) 0x00);
    }

    public static ArrArrayRec fromBytes(byte[] bytes) {
        ArrArrayRec r = new ArrArrayRec();
        System.arraycopy(bytes, 0, r.data, 0, LENGTH);
        return r;
    }

    /** ARR-ACCT-CURR-BAL(index), index 1..5. */
    public static FieldDef arrAcctCurrBal(int index) {
        checkIndex(index);
        return FieldDef.sn("ARR-ACCT-CURR-BAL(" + index + ")", 11 + (index - 1) * ENTRY_LENGTH, 10, 2);
    }

    /** ARR-ACCT-CURR-CYC-DEBIT(index), index 1..5. */
    public static FieldDef arrAcctCurrCycDebit(int index) {
        checkIndex(index);
        return FieldDef.packed("ARR-ACCT-CURR-CYC-DEBIT(" + index + ")", 11 + (index - 1) * ENTRY_LENGTH + 12, 10, 2);
    }

    private static void checkIndex(int index) {
        if (index < 1 || index > OCCURS) {
            throw new IndexOutOfBoundsException("ARR-ACCT-BAL subscript " + index + " out of range 1.." + OCCURS);
        }
    }

    /** INITIALIZE ARR-ARRAY-REC. */
    public void initializeRecord() {
        initialize(ARR_ACCT_ID);
        for (int i = 1; i <= OCCURS; i++) {
            initialize(arrAcctCurrBal(i));
            initialize(arrAcctCurrCycDebit(i));
        }
        initialize(ARR_FILLER);
    }
}
