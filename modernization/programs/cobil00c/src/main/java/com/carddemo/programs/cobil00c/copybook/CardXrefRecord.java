package com.carddemo.programs.cobil00c.copybook;

/** CVACT03Y {@code CARD-XREF-RECORD} (RECLN 50). Read here through the CXACAIX alternate index on XREF-ACCT-ID. */
public record CardXrefRecord(
        String xrefCardNum,   // PIC X(16)
        long xrefCustId,      // PIC 9(09)
        long xrefAcctId) {    // PIC 9(11); FILLER X(14) dropped

    public static final int RECORD_LENGTH = 50;
    public static final int ACCT_ID_LENGTH = 11;

    public CardXrefRecord {
        xrefCardNum = Cobol.alnum(xrefCardNum, 16);
    }

    /** CXACAIX alternate key bytes (XREF-ACCT-ID as PIC 9(11) display). */
    public String acctKey() {
        return Cobol.unsignedDigits(xrefAcctId, ACCT_ID_LENGTH);
    }
}
