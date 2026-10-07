package com.carddemo.batch.record;

import com.carddemo.batch.codec.Field;
import com.carddemo.batch.codec.FixedWidth;
import com.carddemo.batch.codec.Layout;

/** {@code 01 VBRC-REC1} of CBACT01C: the 12-byte variable record (account id + status). */
public final class VbrcRec1 extends FixedWidthRecord {

    private static final Layout.Builder B = Layout.builder("VBRC-REC1");
    public static final Field VB1_ACCT_ID = B.unsigned("VB1-ACCT-ID", 11);
    public static final Field VB1_ACCT_ACTIVE_STATUS = B.text("VB1-ACCT-ACTIVE-STATUS", 1);
    public static final Layout LAYOUT = B.build();
    public static final int LENGTH = LAYOUT.length();

    public VbrcRec1() {
        super(LAYOUT);
    }

    private VbrcRec1(byte[] raw) {
        super(LAYOUT, raw);
    }

    public static VbrcRec1 decode(byte[] raw) {
        return new VbrcRec1(raw);
    }

    @Override
    public Layout layout() {
        return LAYOUT;
    }

    public long vb1AcctId() {
        return FixedWidth.unsigned(data, VB1_ACCT_ID);
    }

    public void setVb1AcctId(long v) {
        FixedWidth.setUnsigned(data, VB1_ACCT_ID, v);
    }

    public String vb1AcctActiveStatus() {
        return FixedWidth.text(data, VB1_ACCT_ACTIVE_STATUS);
    }

    public void setVb1AcctActiveStatus(String v) {
        FixedWidth.setText(data, VB1_ACCT_ACTIVE_STATUS, v);
    }
}
