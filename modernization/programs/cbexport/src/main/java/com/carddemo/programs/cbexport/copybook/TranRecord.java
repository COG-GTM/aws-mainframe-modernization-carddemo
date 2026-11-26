package com.carddemo.programs.cbexport.copybook;

import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.getAlnum;
import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.getSignedZoned;
import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.getUnsignedZoned;
import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.putAlnum;
import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.putSignedZoned;
import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.putUnsignedZoned;

import java.math.BigDecimal;

/**
 * CVTRA05Y - 01 TRAN-RECORD (RECLN 350).
 *
 * @param tranId          TRAN-ID PIC X(16)
 * @param typeCd          TRAN-TYPE-CD PIC X(02)
 * @param catCd           TRAN-CAT-CD PIC 9(04)
 * @param source          TRAN-SOURCE PIC X(10)
 * @param desc            TRAN-DESC PIC X(100)
 * @param amt             TRAN-AMT PIC S9(09)V99
 * @param merchantId      TRAN-MERCHANT-ID PIC 9(09)
 * @param merchantName    TRAN-MERCHANT-NAME PIC X(50)
 * @param merchantCity    TRAN-MERCHANT-CITY PIC X(50)
 * @param merchantZip     TRAN-MERCHANT-ZIP PIC X(10)
 * @param cardNum         TRAN-CARD-NUM PIC X(16)
 * @param origTs          TRAN-ORIG-TS PIC X(26)
 * @param procTs          TRAN-PROC-TS PIC X(26)
 * @param filler          FILLER PIC X(20)
 */
public record TranRecord(
        String tranId,
        String typeCd,
        int catCd,
        String source,
        String desc,
        BigDecimal amt,
        long merchantId,
        String merchantName,
        String merchantCity,
        String merchantZip,
        String cardNum,
        String origTs,
        String procTs,
        String filler) {

    public static final int LENGTH = 350;
    public static final int AMT_SCALE = 2;

    public static TranRecord parse(byte[] r) {
        return new TranRecord(
                getAlnum(r, 0, 16),
                getAlnum(r, 16, 2),
                (int) getUnsignedZoned(r, 18, 4),
                getAlnum(r, 22, 10),
                getAlnum(r, 32, 100),
                getSignedZoned(r, 132, 11, AMT_SCALE),
                getUnsignedZoned(r, 143, 9),
                getAlnum(r, 152, 50),
                getAlnum(r, 202, 50),
                getAlnum(r, 252, 10),
                getAlnum(r, 262, 16),
                getAlnum(r, 278, 26),
                getAlnum(r, 304, 26),
                getAlnum(r, 330, 20));
    }

    public byte[] toBytes() {
        byte[] r = new byte[LENGTH];
        putAlnum(r, 0, 16, tranId);
        putAlnum(r, 16, 2, typeCd);
        putUnsignedZoned(r, 18, 4, catCd);
        putAlnum(r, 22, 10, source);
        putAlnum(r, 32, 100, desc);
        putSignedZoned(r, 132, 11, AMT_SCALE, amt);
        putUnsignedZoned(r, 143, 9, merchantId);
        putAlnum(r, 152, 50, merchantName);
        putAlnum(r, 202, 50, merchantCity);
        putAlnum(r, 252, 10, merchantZip);
        putAlnum(r, 262, 16, cardNum);
        putAlnum(r, 278, 26, origTs);
        putAlnum(r, 304, 26, procTs);
        putAlnum(r, 330, 20, filler);
        return r;
    }
}
