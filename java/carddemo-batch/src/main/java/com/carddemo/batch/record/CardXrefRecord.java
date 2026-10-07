package com.carddemo.batch.record;

import com.carddemo.batch.codec.Field;
import com.carddemo.batch.codec.FixedWidth;
import com.carddemo.batch.codec.Layout;

/**
 * Copybook {@code CVACT03Y} - {@code CARD-XREF-RECORD} (RECLN 50), the XREFFILE KSDS record (key XREF-CARD-NUM).
 * <pre>
 * 05  XREF-CARD-NUM               PIC X(16).
 * 05  XREF-CUST-ID                PIC 9(09).
 * 05  XREF-ACCT-ID                PIC 9(11).
 * 05  FILLER                      PIC X(14).
 * </pre>
 */
public final class CardXrefRecord extends FixedWidthRecord {

    private static final Layout.Builder B = Layout.builder("CARD-XREF-RECORD");
    public static final Field XREF_CARD_NUM = B.text("XREF-CARD-NUM", 16);
    public static final Field XREF_CUST_ID = B.unsigned("XREF-CUST-ID", 9);
    public static final Field XREF_ACCT_ID = B.unsigned("XREF-ACCT-ID", 11);
    public static final Field FILLER = B.filler(14);
    public static final Layout LAYOUT = B.build();
    public static final int LENGTH = LAYOUT.length();

    public CardXrefRecord() {
        super(LAYOUT);
    }

    private CardXrefRecord(byte[] raw) {
        super(LAYOUT, raw);
    }

    public static CardXrefRecord decode(byte[] raw) {
        return new CardXrefRecord(raw);
    }

    @Override
    public Layout layout() {
        return LAYOUT;
    }

    /** COBOL {@code READ ... INTO CARD-XREF-RECORD}: replace the whole buffer. */
    public void moveFrom(byte[] raw) {
        System.arraycopy(FixedWidth.decode(LAYOUT, raw), 0, data, 0, data.length);
    }

    public String xrefCardNum() {
        return FixedWidth.text(data, XREF_CARD_NUM);
    }

    public void setXrefCardNum(String v) {
        FixedWidth.setText(data, XREF_CARD_NUM, v);
    }

    public long xrefCustId() {
        return FixedWidth.unsigned(data, XREF_CUST_ID);
    }

    public void setXrefCustId(long v) {
        FixedWidth.setUnsigned(data, XREF_CUST_ID, v);
    }

    public long xrefAcctId() {
        return FixedWidth.unsigned(data, XREF_ACCT_ID);
    }

    public void setXrefAcctId(long v) {
        FixedWidth.setUnsigned(data, XREF_ACCT_ID, v);
    }

    /** What {@code DISPLAY field} prints for one field (signed numerics get GnuCOBOL's trailing sign). */
    public String display(Field f) {
        return FixedWidth.display(data, f);
    }
}
