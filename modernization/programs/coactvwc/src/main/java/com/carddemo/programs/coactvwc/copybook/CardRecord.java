package com.carddemo.programs.coactvwc.copybook;

import com.carddemo.programs.coactvwc.support.Cobol;

/** CVACT02Y: {@code CARD-RECORD} (RECLN 150). COPYed by COACTVWC but not read by it. */
public record CardRecord(
        String cardNum,            // X(16)
        long cardAcctId,           // 9(11)
        int cardCvvCd,             // 9(03)
        String cardEmbossedName,   // X(50)
        String cardExpirationDate, // X(10) (COBOL name CARD-EXPIRAION-DATE)
        String cardActiveStatus) { // X(01); FILLER X(59) dropped
    public static final int LENGTH = 150;

    public static CardRecord parse(String line) {
        String s = Cobol.alnum(line, LENGTH);
        return new CardRecord(s.substring(0, 16), Cobol.toLong(s.substring(16, 27)),
                (int) Cobol.toLong(s.substring(27, 30)), s.substring(30, 80), s.substring(80, 90), s.substring(90, 91));
    }

    public String toFixedWidth() {
        return Cobol.alnum(cardNum, 16) + Cobol.digits(cardAcctId, 11) + Cobol.digits(cardCvvCd, 3)
                + Cobol.alnum(cardEmbossedName, 50) + Cobol.alnum(cardExpirationDate, 10)
                + Cobol.alnum(cardActiveStatus, 1) + Cobol.spaces(59);
    }
}
