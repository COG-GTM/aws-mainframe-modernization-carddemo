package com.carddemo.batch.record;

import com.carddemo.batch.codec.Field;
import com.carddemo.batch.codec.FixedWidth;
import com.carddemo.batch.codec.Layout;

import java.math.BigDecimal;

/** {@code 01 VBRC-REC2} of CBACT01C: the 39-byte variable record (id, balance, limit, reissue year). */
public final class VbrcRec2 extends FixedWidthRecord {

    private static final Layout.Builder B = Layout.builder("VBRC-REC2");
    public static final Field VB2_ACCT_ID = B.unsigned("VB2-ACCT-ID", 11);
    public static final Field VB2_ACCT_CURR_BAL = B.zoned("VB2-ACCT-CURR-BAL", 10, 2);
    public static final Field VB2_ACCT_CREDIT_LIMIT = B.zoned("VB2-ACCT-CREDIT-LIMIT", 10, 2);
    public static final Field VB2_ACCT_REISSUE_YYYY = B.text("VB2-ACCT-REISSUE-YYYY", 4);
    public static final Layout LAYOUT = B.build();
    public static final int LENGTH = LAYOUT.length();

    public VbrcRec2() {
        super(LAYOUT);
    }

    private VbrcRec2(byte[] raw) {
        super(LAYOUT, raw);
    }

    public static VbrcRec2 decode(byte[] raw) {
        return new VbrcRec2(raw);
    }

    @Override
    public Layout layout() {
        return LAYOUT;
    }

    public long vb2AcctId() {
        return FixedWidth.unsigned(data, VB2_ACCT_ID);
    }

    public void setVb2AcctId(long v) {
        FixedWidth.setUnsigned(data, VB2_ACCT_ID, v);
    }

    public BigDecimal vb2AcctCurrBal() {
        return FixedWidth.decimal(data, VB2_ACCT_CURR_BAL);
    }

    public void setVb2AcctCurrBal(BigDecimal v) {
        FixedWidth.setDecimal(data, VB2_ACCT_CURR_BAL, v);
    }

    public BigDecimal vb2AcctCreditLimit() {
        return FixedWidth.decimal(data, VB2_ACCT_CREDIT_LIMIT);
    }

    public void setVb2AcctCreditLimit(BigDecimal v) {
        FixedWidth.setDecimal(data, VB2_ACCT_CREDIT_LIMIT, v);
    }

    /** The reissue year: text, first four characters of {@code ACCT-REISSUE-DATE}. */
    public String vb2AcctReissueYyyy() {
        return FixedWidth.text(data, VB2_ACCT_REISSUE_YYYY);
    }

    public void setVb2AcctReissueYyyy(String v) {
        FixedWidth.setText(data, VB2_ACCT_REISSUE_YYYY, v);
    }
}
