package com.carddemo.programs.cbimport.copybook;

/** CVACT02Y: CARD-RECORD, RECLN 150 (CARDOUT, LRECL=150). FILLER X(59) written as spaces. */
public record CardRecord(
        String cardNum,                 // CARD-NUM X(16)
        long acctId,                    // CARD-ACCT-ID 9(11)
        int cvvCd,                      // CARD-CVV-CD 9(03)
        String embossedName,            // CARD-EMBOSSED-NAME X(50)
        String expirationDate,          // CARD-EXPIRAION-DATE X(10)
        String activeStatus) {          // CARD-ACTIVE-STATUS X(01)

    public static final int LENGTH = 150;
    public static final int FILLER_LENGTH = 59;

    public CardRecord {
        cardNum = Pic.alnum(cardNum, 16);
        acctId = Pic.unsigned(acctId, 11);
        cvvCd = (int) Pic.unsigned(cvvCd, 3);
        embossedName = Pic.alnum(embossedName, 50);
        expirationDate = Pic.alnum(expirationDate, 10);
        activeStatus = Pic.alnum(activeStatus, 1);
    }

    /** INITIALIZE CARD-RECORD. */
    public static CardRecord initialized() {
        return new CardRecord("", 0, 0, "", "", "");
    }

    public String toFixedWidth() {
        return cardNum + Pic.displayUnsigned(acctId, 11) + Pic.displayUnsigned(cvvCd, 3)
                + embossedName + expirationDate + activeStatus + " ".repeat(FILLER_LENGTH);
    }

    public static CardRecord parse(String line) {
        Slicer s = new Slicer(line, LENGTH);
        return new CardRecord(s.x(16), s.unsigned(11), (int) s.unsigned(3), s.x(50), s.x(10), s.x(1));
    }
}
