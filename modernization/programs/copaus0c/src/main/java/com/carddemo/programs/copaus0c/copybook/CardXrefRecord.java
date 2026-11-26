package com.carddemo.programs.copaus0c.copybook;

/** CVACT03Y - CARD-XREF-RECORD (RECLN 50). FILLER X(14) dropped. */
public record CardXrefRecord(
        String xrefCardNum, // XREF-CARD-NUM PIC X(16)
        long xrefCustId,    // XREF-CUST-ID  PIC 9(09)
        long xrefAcctId) {  // XREF-ACCT-ID  PIC 9(11)

    public static final int RECORD_LENGTH = 50;

    public static CardXrefRecord parse(String line) {
        String r = String.format("%-" + RECORD_LENGTH + "s", line);
        return new CardXrefRecord(r.substring(0, 16), Long.parseLong(r.substring(16, 25)),
                Long.parseLong(r.substring(25, 36)));
    }
}
