package com.carddemo.programs.cbtrn01c.copybook;

/** Copybook CVACT02Y: 01 CARD-RECORD (RECLN 150). */
public record CardRecord(
        String cardNum,         // CARD-NUM            PIC X(16)
        long acctId,            // CARD-ACCT-ID        PIC 9(11)
        long cvvCd,             // CARD-CVV-CD         PIC 9(03)
        String embossedName,    // CARD-EMBOSSED-NAME  PIC X(50)
        String expiraionDate,   // CARD-EXPIRAION-DATE PIC X(10) (sic, as in copybook)
        String activeStatus,    // CARD-ACTIVE-STATUS  PIC X(01)
        String filler) {        // FILLER              PIC X(59)

    public static final int LENGTH = 150;

    public CardRecord {
        cardNum = Pic.alpha(cardNum, 16);
        acctId = Pic.unsigned(acctId, 11);
        cvvCd = Pic.unsigned(cvvCd, 3);
        embossedName = Pic.alpha(embossedName, 50);
        expiraionDate = Pic.alpha(expiraionDate, 10);
        activeStatus = Pic.alpha(activeStatus, 1);
        filler = Pic.alpha(filler, 59);
    }

    public static CardRecord initial() {
        return new CardRecord("", 0, 0, "", "", "", "");
    }

    public static final RecordCodec<CardRecord> CODEC = new RecordCodec<>() {
        @Override
        public int length() {
            return LENGTH;
        }

        @Override
        public CardRecord parse(String image) {
            Pic.Reader r = new Pic.Reader(image, LENGTH);
            return new CardRecord(r.x(16), r.n(11), r.n(3), r.x(50), r.x(10), r.x(1), r.x(59));
        }

        @Override
        public String format(CardRecord c) {
            return c.cardNum + Pic.formatUnsigned(c.acctId, 11) + Pic.formatUnsigned(c.cvvCd, 3)
                    + c.embossedName + c.expiraionDate + c.activeStatus + c.filler;
        }
    };

    public String toFixedWidth() {
        return CODEC.format(this);
    }
}
