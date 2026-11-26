package com.carddemo.programs.cbact04c.copybook;

/**
 * CVACT03Y - CARD-XREF-RECORD (RECLN 50), card/customer/account cross reference (XREFFILE KSDS,
 * alternate key XREF-ACCT-ID via the XREFFIL1 AIX path).
 *
 * <pre>
 * 05 XREF-CARD-NUM   PIC X(16).
 * 05 XREF-CUST-ID    PIC 9(09).
 * 05 XREF-ACCT-ID    PIC 9(11).
 * 05 FILLER          PIC X(14).
 * </pre>
 */
public record CardXrefRecord(String xrefCardNum, long xrefCustId, long xrefAcctId, String filler) {

    public static final int LENGTH = 50;

    public CardXrefRecord {
        xrefCardNum = Pic.alpha(xrefCardNum, 16);
        filler = Pic.alpha(filler, 14);
    }

    public CardXrefRecord(String cardNum, long custId, long acctId) {
        this(cardNum, custId, acctId, "");
    }

    public static final RecordCodec<CardXrefRecord> CODEC = new RecordCodec<>() {
        @Override
        public int length() {
            return LENGTH;
        }

        @Override
        public CardXrefRecord decode(String image) {
            Pic.Cursor c = new Pic.Cursor(image, LENGTH);
            return new CardXrefRecord(
                    c.take(16),
                    Pic.parseUnsigned(c.take(9)),
                    Pic.parseUnsigned(c.take(11)),
                    c.take(14));
        }

        @Override
        public String encode(CardXrefRecord r) {
            return r.xrefCardNum
                    + Pic.unsigned(r.xrefCustId, 9)
                    + Pic.unsigned(r.xrefAcctId, 11)
                    + r.filler;
        }
    };
}
