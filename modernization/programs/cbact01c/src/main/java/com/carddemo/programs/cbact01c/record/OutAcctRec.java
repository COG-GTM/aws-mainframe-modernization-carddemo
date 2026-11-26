package com.carddemo.programs.cbact01c.record;

import com.carddemo.programs.cbact01c.support.FieldDef;
import com.carddemo.programs.cbact01c.support.RecordBuffer;

import java.util.List;

/**
 * {@code FD OUT-FILE / 01 OUT-ACCT-REC} (DD OUTFILE, LRECL=107 RECFM=FB). One instance is the FD record area
 * for the whole run, so a field the program does not MOVE keeps its previous content (see
 * OUT-ACCT-CURR-CYC-DEBIT in 1300-POPUL-ACCT-RECORD). The area starts as low-values.
 */
public final class OutAcctRec extends RecordBuffer {
    public static final int LENGTH = 107;

    public static final FieldDef OUT_ACCT_ID = FieldDef.n("OUT-ACCT-ID", 0, 11);
    public static final FieldDef OUT_ACCT_ACTIVE_STATUS = FieldDef.x("OUT-ACCT-ACTIVE-STATUS", 11, 1);
    public static final FieldDef OUT_ACCT_CURR_BAL = FieldDef.sn("OUT-ACCT-CURR-BAL", 12, 10, 2);
    public static final FieldDef OUT_ACCT_CREDIT_LIMIT = FieldDef.sn("OUT-ACCT-CREDIT-LIMIT", 24, 10, 2);
    public static final FieldDef OUT_ACCT_CASH_CREDIT_LIMIT = FieldDef.sn("OUT-ACCT-CASH-CREDIT-LIMIT", 36, 10, 2);
    public static final FieldDef OUT_ACCT_OPEN_DATE = FieldDef.x("OUT-ACCT-OPEN-DATE", 48, 10);
    public static final FieldDef OUT_ACCT_EXPIRAION_DATE = FieldDef.x("OUT-ACCT-EXPIRAION-DATE", 58, 10);
    public static final FieldDef OUT_ACCT_REISSUE_DATE = FieldDef.x("OUT-ACCT-REISSUE-DATE", 68, 10);
    public static final FieldDef OUT_ACCT_CURR_CYC_CREDIT = FieldDef.sn("OUT-ACCT-CURR-CYC-CREDIT", 78, 10, 2);
    public static final FieldDef OUT_ACCT_CURR_CYC_DEBIT = FieldDef.packed("OUT-ACCT-CURR-CYC-DEBIT", 90, 10, 2);
    public static final FieldDef OUT_ACCT_GROUP_ID = FieldDef.x("OUT-ACCT-GROUP-ID", 97, 10);

    public static final List<FieldDef> FIELDS = List.of(OUT_ACCT_ID, OUT_ACCT_ACTIVE_STATUS, OUT_ACCT_CURR_BAL,
            OUT_ACCT_CREDIT_LIMIT, OUT_ACCT_CASH_CREDIT_LIMIT, OUT_ACCT_OPEN_DATE, OUT_ACCT_EXPIRAION_DATE,
            OUT_ACCT_REISSUE_DATE, OUT_ACCT_CURR_CYC_CREDIT, OUT_ACCT_CURR_CYC_DEBIT, OUT_ACCT_GROUP_ID);

    public OutAcctRec() {
        super(LENGTH, (byte) 0x00);
    }

    public static OutAcctRec fromBytes(byte[] bytes) {
        OutAcctRec r = new OutAcctRec();
        System.arraycopy(bytes, 0, r.data, 0, LENGTH);
        return r;
    }
}
