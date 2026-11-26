package com.carddemo.programs.cbexport.copybook;

import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.getAlnum;
import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.getUnsignedZoned;
import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.putAlnum;
import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.putUnsignedZoned;

/**
 * CVACT03Y - 01 CARD-XREF-RECORD (RECLN 50).
 *
 * @param cardNum XREF-CARD-NUM PIC X(16)
 * @param custId  XREF-CUST-ID PIC 9(09)
 * @param acctId  XREF-ACCT-ID PIC 9(11)
 * @param filler  FILLER PIC X(14)
 */
public record CardXrefRecord(String cardNum, long custId, long acctId, String filler) {

    public static final int LENGTH = 50;

    public static CardXrefRecord parse(byte[] r) {
        return new CardXrefRecord(
                getAlnum(r, 0, 16),
                getUnsignedZoned(r, 16, 9),
                getUnsignedZoned(r, 25, 11),
                getAlnum(r, 36, 14));
    }

    public byte[] toBytes() {
        byte[] r = new byte[LENGTH];
        putAlnum(r, 0, 16, cardNum);
        putUnsignedZoned(r, 16, 9, custId);
        putUnsignedZoned(r, 25, 11, acctId);
        putAlnum(r, 36, 14, filler);
        return r;
    }
}
