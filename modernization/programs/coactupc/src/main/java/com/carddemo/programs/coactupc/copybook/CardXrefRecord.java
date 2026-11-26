package com.carddemo.programs.coactupc.copybook;

import com.carddemo.programs.coactupc.support.Cobol;
import com.carddemo.programs.coactupc.support.FixedWidth;

/**
 * CVACT03Y - CARD-XREF-RECORD (CARDXREF KSDS RECLN 50; read here through the CXACAIX
 * alternate index keyed on XREF-ACCT-ID).
 *
 * @param cardNum XREF-CARD-NUM PIC X(16)
 * @param custId  XREF-CUST-ID PIC 9(09)
 * @param acctId  XREF-ACCT-ID PIC 9(11)
 * @param filler  FILLER PIC X(14)
 */
public record CardXrefRecord(String cardNum, long custId, long acctId, String filler) {

    public static final int RECORD_LENGTH = 50;

    public static CardXrefRecord parse(String image) {
        FixedWidth f = new FixedWidth(image, RECORD_LENGTH);
        return new CardXrefRecord(f.next(16), Cobol.unsignedValue(f.next(9)), Cobol.unsignedValue(f.next(11)),
                f.next(14));
    }

    public static CardXrefRecord lowValues() {
        return parse(Cobol.lowValues(RECORD_LENGTH));
    }

    public String toFixedWidth() {
        return Cobol.pad(cardNum, 16) + Cobol.unsignedDisplay(custId, 9) + Cobol.unsignedDisplay(acctId, 11)
                + Cobol.pad(filler, 14);
    }

    /** Alternate key image (XREF-ACCT-ID as 11 display digits). */
    public String acctKey() {
        return Cobol.unsignedDisplay(acctId, 11);
    }
}
