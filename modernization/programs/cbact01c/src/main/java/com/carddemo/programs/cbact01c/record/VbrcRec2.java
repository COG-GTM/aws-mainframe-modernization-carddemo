package com.carddemo.programs.cbact01c.record;

import com.carddemo.programs.cbact01c.support.FieldDef;
import com.carddemo.programs.cbact01c.support.RecordBuffer;

/** WORKING-STORAGE {@code 01 VBRC-REC2}: the 39-byte long record written to the variable-length VBRC file. */
public final class VbrcRec2 extends RecordBuffer {
    public static final int LENGTH = 39;

    public static final FieldDef VB2_ACCT_ID = FieldDef.n("VB2-ACCT-ID", 0, 11);
    public static final FieldDef VB2_ACCT_CURR_BAL = FieldDef.sn("VB2-ACCT-CURR-BAL", 11, 10, 2);
    public static final FieldDef VB2_ACCT_CREDIT_LIMIT = FieldDef.sn("VB2-ACCT-CREDIT-LIMIT", 23, 10, 2);
    public static final FieldDef VB2_ACCT_REISSUE_YYYY = FieldDef.x("VB2-ACCT-REISSUE-YYYY", 35, 4);

    public VbrcRec2() {
        super(LENGTH, (byte) ' ');
    }
}
