package com.carddemo.programs.cbtrn02c.copybook;

/** CVACT03Y: CARD-XREF-RECORD, card to customer/account cross reference (RECLN 50). Key XREF-CARD-NUM. */
public record CardXrefRecord(
        String xrefCardNum, // XREF-CARD-NUM PIC X(16)
        long xrefCustId,    // XREF-CUST-ID  PIC 9(09)
        long xrefAcctId,    // XREF-ACCT-ID  PIC 9(11)
        String filler) {    // FILLER        PIC X(14)

    public static final int RECORD_LENGTH = 50;
    public static final int KEY_OFFSET = 0;
    public static final int KEY_LENGTH = 16;

    public static CardXrefRecord parse(String raw) {
        Pic.Reader r = new Pic.Reader(raw, RECORD_LENGTH);
        return new CardXrefRecord(r.x(16), r.n(9), r.n(11), r.x(14));
    }

    public String format() {
        return new Pic.Writer().x(xrefCardNum, 16).n(xrefCustId, 9).n(xrefAcctId, 11).x(filler, 14)
                .build(RECORD_LENGTH);
    }
}
