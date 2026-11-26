package com.carddemo.programs.cbstm03a.copybook;

import com.carddemo.programs.cbstm03a.support.Pic;

/** CVACT03Y - CARD-XREF-RECORD (RECLN 50). */
public record CardXrefRecord(
        String xrefCardNum, // XREF-CARD-NUM  PIC X(16)
        long xrefCustId,    // XREF-CUST-ID   PIC 9(09)
        long xrefAcctId,    // XREF-ACCT-ID   PIC 9(11)
        String filler) {    // FILLER         PIC X(14)

    public static final int LENGTH = 50;
    public static final int CUST_ID_LENGTH = 9;
    public static final int ACCT_ID_LENGTH = 11;

    public static CardXrefRecord parse(String raw) {
        String r = Pic.x(raw, LENGTH);
        return new CardXrefRecord(
                r.substring(0, 16),
                Pic.unsigned(r.substring(16, 25)),
                Pic.unsigned(r.substring(25, 36)),
                r.substring(36, 50));
    }

    /** XREF-CUST-ID as its 9 DISPLAY digits. */
    public String xrefCustIdDisplay() {
        return Pic.unsigned(xrefCustId, CUST_ID_LENGTH);
    }

    /** XREF-ACCT-ID as its 11 DISPLAY digits. */
    public String xrefAcctIdDisplay() {
        return Pic.unsigned(xrefAcctId, ACCT_ID_LENGTH);
    }

    public String toFixedWidth() {
        return Pic.x(xrefCardNum, 16) + xrefCustIdDisplay() + xrefAcctIdDisplay() + Pic.x(filler, 14);
    }
}
