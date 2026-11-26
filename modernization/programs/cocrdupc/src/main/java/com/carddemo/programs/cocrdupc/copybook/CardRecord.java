package com.carddemo.programs.cocrdupc.copybook;

import static com.carddemo.programs.cocrdupc.Cobol.alnum;
import static com.carddemo.programs.cocrdupc.Cobol.displayValue;
import static com.carddemo.programs.cocrdupc.Cobol.numeric;

/** CVACT02Y: 01 CARD-RECORD (150 bytes, VSAM KSDS CARDDAT keyed on CARD-NUM). */
public record CardRecord(
        String cardNum,           // X(16)
        long cardAcctId,          // 9(11)
        int cardCvvCd,            // 9(03)
        String cardEmbossedName,  // X(50)
        String cardExpiraionDate, // X(10)  YYYY-MM-DD
        String cardActiveStatus,  // X(01)
        String filler             // X(59)
) {
    public static final int LENGTH = 150;

    public CardRecord {
        cardNum = alnum(cardNum, 16);
        cardAcctId = displayValue(numeric(cardAcctId, 11));
        cardCvvCd = (int) displayValue(numeric(cardCvvCd, 3));
        cardEmbossedName = alnum(cardEmbossedName, 50);
        cardExpiraionDate = alnum(cardExpiraionDate, 10);
        cardActiveStatus = alnum(cardActiveStatus, 1);
        filler = alnum(filler, 59);
    }

    public static CardRecord parse(String raw) {
        String r = alnum(raw, LENGTH);
        return new CardRecord(r.substring(0, 16), displayValue(r.substring(16, 27)),
                (int) displayValue(r.substring(27, 30)), r.substring(30, 80), r.substring(80, 90),
                r.substring(90, 91), r.substring(91, 150));
    }

    public String toFixedWidth() {
        return cardNum + numeric(cardAcctId, 11) + numeric(cardCvvCd, 3) + cardEmbossedName
                + cardExpiraionDate + cardActiveStatus + filler;
    }

    /** CARD-CVV-CD as its DISPLAY bytes. */
    public String cardCvvCdDisplay() {
        return numeric(cardCvvCd, 3);
    }
}
