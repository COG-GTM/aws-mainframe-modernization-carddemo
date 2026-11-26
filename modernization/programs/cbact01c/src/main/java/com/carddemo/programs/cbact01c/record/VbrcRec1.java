package com.carddemo.programs.cbact01c.record;

import com.carddemo.programs.cbact01c.support.FieldDef;
import com.carddemo.programs.cbact01c.support.RecordBuffer;

/** WORKING-STORAGE {@code 01 VBRC-REC1}: the 12-byte short record written to the variable-length VBRC file. */
public final class VbrcRec1 extends RecordBuffer {
    public static final int LENGTH = 12;

    public static final FieldDef VB1_ACCT_ID = FieldDef.n("VB1-ACCT-ID", 0, 11);
    public static final FieldDef VB1_ACCT_ACTIVE_STATUS = FieldDef.x("VB1-ACCT-ACTIVE-STATUS", 11, 1);

    public VbrcRec1() {
        super(LENGTH, (byte) ' ');
    }

    /** INITIALIZE VBRC-REC1. */
    public void initializeRecord() {
        initialize(VB1_ACCT_ID);
        initialize(VB1_ACCT_ACTIVE_STATUS);
    }
}
