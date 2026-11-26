package com.carddemo.programs.coactvwc.copybook;

import com.carddemo.programs.coactvwc.support.Cobol;

/** CVACT03Y: {@code CARD-XREF-RECORD} (RECLN 50). */
public record CardXrefRecord(
        String xrefCardNum, // X(16)
        long xrefCustId,    // 9(09)
        long xrefAcctId) {  // 9(11); FILLER X(14) dropped
    public static final int LENGTH = 50;

    public static CardXrefRecord parse(String line) {
        String s = Cobol.alnum(line, LENGTH);
        return new CardXrefRecord(s.substring(0, 16), Cobol.toLong(s.substring(16, 25)),
                Cobol.toLong(s.substring(25, 36)));
    }

    /** Alternate-index key (CXACAIX: KEYS(11,25)) as text. */
    public String acctIdKey() {
        return Cobol.digits(xrefAcctId, 11);
    }

    public String toFixedWidth() {
        return Cobol.alnum(xrefCardNum, 16) + Cobol.digits(xrefCustId, 9) + Cobol.digits(xrefAcctId, 11)
                + Cobol.spaces(14);
    }
}
