package com.carddemo.programs.cbtrn03c.copybook;

import com.carddemo.programs.cbtrn03c.support.Fixed;

/**
 * CVACT03Y - 01 CARD-XREF-RECORD (RECLN 50), key XREF-CARD-NUM.
 *
 * <pre>
 * 05 XREF-CARD-NUM PIC X(16)
 * 05 XREF-CUST-ID  PIC 9(09)
 * 05 XREF-ACCT-ID  PIC 9(11)
 * 05 FILLER        PIC X(14)
 * </pre>
 */
public record CardXrefRecord(String xrefCardNum, long xrefCustId, long xrefAcctId, String filler) {

    public static final int LENGTH = 50;
    public static final int KEY_OFFSET = 0;
    public static final int KEY_LENGTH = 16;

    public CardXrefRecord {
        xrefCardNum = Fixed.pic(xrefCardNum, 16);
        filler = Fixed.pic(filler, 14);
    }

    public static CardXrefRecord initial() {
        return new CardXrefRecord("", 0L, 0L, "");
    }

    public static CardXrefRecord parse(String record) {
        String r = Fixed.pic(record, LENGTH);
        return new CardXrefRecord(
                r.substring(0, 16),
                Fixed.parseUnsigned(r.substring(16, 25), "XREF-CUST-ID"),
                Fixed.parseUnsigned(r.substring(25, 36), "XREF-ACCT-ID"),
                r.substring(36, 50));
    }

    /** XREF-ACCT-ID as its PIC 9(11) DISPLAY digits. */
    public String xrefAcctIdDisplay() {
        return Fixed.unsigned(xrefAcctId, 11);
    }

    public String toFixedWidth() {
        return xrefCardNum + Fixed.unsigned(xrefCustId, 9) + Fixed.unsigned(xrefAcctId, 11) + filler;
    }
}
