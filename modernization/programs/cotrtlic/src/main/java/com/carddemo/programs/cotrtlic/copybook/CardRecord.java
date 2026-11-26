package com.carddemo.programs.cotrtlic.copybook;

import com.carddemo.programs.cotrtlic.support.Cob;

/** Copybook CVACT02Y: 01 CARD-RECORD (150 bytes). COPYed by COTRTLIC but not referenced. */
public record CardRecord(
        /* CARD-NUM PIC X(16) */ String cardNum,
        /* CARD-ACCT-ID PIC 9(11) */ long acctId,
        /* CARD-CVV-CD PIC 9(03) */ int cvvCd,
        /* CARD-EMBOSSED-NAME PIC X(50) */ String embossedName,
        /* CARD-EXPIRAION-DATE PIC X(10) */ String expirationDate,
        /* CARD-ACTIVE-STATUS PIC X(01) */ String activeStatus) {

    public static final int LENGTH = 150;

    public static CardRecord fromFixed(String s) {
        String r = Cob.move(s, LENGTH);
        return new CardRecord(r.substring(0, 16), Long.parseLong(r.substring(16, 27)),
                Integer.parseInt(r.substring(27, 30)), r.substring(30, 80), r.substring(80, 90),
                r.substring(90, 91));
    }

    /** FILLER PIC X(59) is emitted as spaces. */
    public String toFixed() {
        return Cob.move(cardNum, 16) + Cob.zeroPad(acctId, 11) + Cob.zeroPad(cvvCd, 3)
                + Cob.move(embossedName, 50) + Cob.move(expirationDate, 10) + Cob.move(activeStatus, 1)
                + Cob.spaces(59);
    }
}
