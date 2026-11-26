package com.carddemo.programs.cbexport.copybook;

import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.getAlnum;
import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.getUnsignedZoned;
import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.putAlnum;
import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.putUnsignedZoned;

/**
 * CVACT02Y - 01 CARD-RECORD (RECLN 150).
 *
 * @param cardNum         CARD-NUM PIC X(16)
 * @param acctId          CARD-ACCT-ID PIC 9(11)
 * @param cvvCd           CARD-CVV-CD PIC 9(03)
 * @param embossedName    CARD-EMBOSSED-NAME PIC X(50)
 * @param expiraionDate   CARD-EXPIRAION-DATE PIC X(10) (sic)
 * @param activeStatus    CARD-ACTIVE-STATUS PIC X(01)
 * @param filler          FILLER PIC X(59)
 */
public record CardRecord(
        String cardNum,
        long acctId,
        int cvvCd,
        String embossedName,
        String expiraionDate,
        String activeStatus,
        String filler) {

    public static final int LENGTH = 150;

    public static CardRecord parse(byte[] r) {
        return new CardRecord(
                getAlnum(r, 0, 16),
                getUnsignedZoned(r, 16, 11),
                (int) getUnsignedZoned(r, 27, 3),
                getAlnum(r, 30, 50),
                getAlnum(r, 80, 10),
                getAlnum(r, 90, 1),
                getAlnum(r, 91, 59));
    }

    public byte[] toBytes() {
        byte[] r = new byte[LENGTH];
        putAlnum(r, 0, 16, cardNum);
        putUnsignedZoned(r, 16, 11, acctId);
        putUnsignedZoned(r, 27, 3, cvvCd);
        putAlnum(r, 30, 50, embossedName);
        putAlnum(r, 80, 10, expiraionDate);
        putAlnum(r, 90, 1, activeStatus);
        putAlnum(r, 91, 59, filler);
        return r;
    }
}
