package com.carddemo.programs.copaua0c.copybook;

import com.carddemo.programs.copaua0c.support.Cobol;

/** CVACT03Y - 01 CARD-XREF-RECORD (RECLN 50; FILLER X(14) dropped). */
public record CardXrefRecord(
        String cardNum,   // XREF-CARD-NUM PIC X(16)
        long custId,      // XREF-CUST-ID  PIC 9(09)
        long acctId) {    // XREF-ACCT-ID  PIC 9(11)

    public static final int RECORD_LENGTH = 50;

    public CardXrefRecord {
        cardNum = Cobol.alpha(cardNum, 16);
    }

    /** Parse a fixed-width record (short lines are space-padded to 50). */
    public static CardXrefRecord fromFixed(String line) {
        String r = Cobol.alpha(line, RECORD_LENGTH);
        return new CardXrefRecord(r.substring(0, 16), Cobol.unsigned(r.substring(16, 25)),
                Cobol.unsigned(r.substring(25, 36)));
    }
}
