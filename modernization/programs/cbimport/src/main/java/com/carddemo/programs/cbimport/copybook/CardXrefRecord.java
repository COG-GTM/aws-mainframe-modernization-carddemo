package com.carddemo.programs.cbimport.copybook;

/** CVACT03Y: CARD-XREF-RECORD, RECLN 50 (XREFOUT, LRECL=50). FILLER X(14) written as spaces. */
public record CardXrefRecord(
        String cardNum,                 // XREF-CARD-NUM X(16)
        long custId,                    // XREF-CUST-ID 9(09)
        long acctId) {                  // XREF-ACCT-ID 9(11)

    public static final int LENGTH = 50;
    public static final int FILLER_LENGTH = 14;

    public CardXrefRecord {
        cardNum = Pic.alnum(cardNum, 16);
        custId = Pic.unsigned(custId, 9);
        acctId = Pic.unsigned(acctId, 11);
    }

    /** INITIALIZE CARD-XREF-RECORD. */
    public static CardXrefRecord initialized() {
        return new CardXrefRecord("", 0, 0);
    }

    public String toFixedWidth() {
        return cardNum + Pic.displayUnsigned(custId, 9) + Pic.displayUnsigned(acctId, 11)
                + " ".repeat(FILLER_LENGTH);
    }

    public static CardXrefRecord parse(String line) {
        Slicer s = new Slicer(line, LENGTH);
        return new CardXrefRecord(s.x(16), s.unsigned(9), s.unsigned(11));
    }
}
