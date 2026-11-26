package com.carddemo.programs.cotrn02c.copybook;

/** CVACT03Y {@code CARD-XREF-RECORD}, RECLN 50. Keyed by XREF-CARD-NUM (CCXREF) and XREF-ACCT-ID (CXACAIX). */
public record CardXrefRecord(
        String cardNum,  // X(16)
        long custId,     // 9(09)
        long acctId,     // 9(11)
        String filler) { // X(14)

    public static final int LENGTH = 50;

    public CardXrefRecord {
        cardNum = Pic.x(cardNum, 16);
        custId = Long.parseLong(Pic.nine(custId, 9));
        acctId = Long.parseLong(Pic.nine(acctId, 11));
        filler = Pic.x(filler, 14);
    }

    public CardXrefRecord(String cardNum, long custId, long acctId) {
        this(cardNum, custId, acctId, "");
    }

    /** Fixed-width parse; short lines (trailing FILLER trimmed in the ASCII extracts) are space-padded. */
    public static CardXrefRecord parse(String line) {
        String r = Pic.x(line, LENGTH);
        return new CardXrefRecord(r.substring(0, 16),
                Pic.alphaToUnsigned(r.substring(16, 25)).longValueExact(),
                Pic.alphaToUnsigned(r.substring(25, 36)).longValueExact(),
                r.substring(36, 50));
    }

    public String toRecord() {
        return cardNum + Pic.nine(custId, 9) + Pic.nine(acctId, 11) + filler;
    }

    /** XREF-ACCT-ID as its PIC 9(11) DISPLAY image. */
    public String acctIdImage() {
        return Pic.nine(acctId, 11);
    }
}
