package com.carddemo.programs.cbtrn01c.copybook;

/** Copybook CVACT03Y: 01 CARD-XREF-RECORD (RECLN 50). */
public record CardXrefRecord(
        String cardNum,     // XREF-CARD-NUM PIC X(16)
        long custId,        // XREF-CUST-ID  PIC 9(09)
        long acctId,        // XREF-ACCT-ID  PIC 9(11)
        String filler) {    // FILLER        PIC X(14)

    public static final int LENGTH = 50;

    public CardXrefRecord {
        cardNum = Pic.alpha(cardNum, 16);
        custId = Pic.unsigned(custId, 9);
        acctId = Pic.unsigned(acctId, 11);
        filler = Pic.alpha(filler, 14);
    }

    public static CardXrefRecord initial() {
        return new CardXrefRecord("", 0, 0, "");
    }

    /** MOVE ... TO XREF-CARD-NUM. */
    public CardXrefRecord withCardNum(String newCardNum) {
        return new CardXrefRecord(newCardNum, custId, acctId, filler);
    }

    public String custIdDisplay() {
        return Pic.formatUnsigned(custId, 9);
    }

    public String acctIdDisplay() {
        return Pic.formatUnsigned(acctId, 11);
    }

    public static final RecordCodec<CardXrefRecord> CODEC = new RecordCodec<>() {
        @Override
        public int length() {
            return LENGTH;
        }

        @Override
        public CardXrefRecord parse(String image) {
            Pic.Reader r = new Pic.Reader(image, LENGTH);
            return new CardXrefRecord(r.x(16), r.n(9), r.n(11), r.x(14));
        }

        @Override
        public String format(CardXrefRecord x) {
            return x.cardNum + Pic.formatUnsigned(x.custId, 9) + Pic.formatUnsigned(x.acctId, 11) + x.filler;
        }
    };

    public String toFixedWidth() {
        return CODEC.format(this);
    }
}
