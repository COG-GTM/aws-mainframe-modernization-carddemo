package com.carddemo.programs.cbact04c.copybook;

import java.math.BigDecimal;

/**
 * CVTRA05Y - TRAN-RECORD (RECLN 350), transaction record (TRANSACT output, LRECL=350).
 *
 * <pre>
 * 05 TRAN-ID              PIC X(16).
 * 05 TRAN-TYPE-CD         PIC X(02).
 * 05 TRAN-CAT-CD          PIC 9(04).
 * 05 TRAN-SOURCE          PIC X(10).
 * 05 TRAN-DESC            PIC X(100).
 * 05 TRAN-AMT             PIC S9(09)V99.
 * 05 TRAN-MERCHANT-ID     PIC 9(09).
 * 05 TRAN-MERCHANT-NAME   PIC X(50).
 * 05 TRAN-MERCHANT-CITY   PIC X(50).
 * 05 TRAN-MERCHANT-ZIP    PIC X(10).
 * 05 TRAN-CARD-NUM        PIC X(16).
 * 05 TRAN-ORIG-TS         PIC X(26).
 * 05 TRAN-PROC-TS         PIC X(26).
 * 05 FILLER               PIC X(20).
 * </pre>
 */
public record TranRecord(
        String tranId,
        String tranTypeCd,
        int tranCatCd,
        String tranSource,
        String tranDesc,
        BigDecimal tranAmt,
        long tranMerchantId,
        String tranMerchantName,
        String tranMerchantCity,
        String tranMerchantZip,
        String tranCardNum,
        String tranOrigTs,
        String tranProcTs,
        String filler) {

    public static final int LENGTH = 350;

    public TranRecord {
        tranId = Pic.alpha(tranId, 16);
        tranTypeCd = Pic.alpha(tranTypeCd, 2);
        tranCatCd = (int) (Math.abs((long) tranCatCd) % 10_000);
        tranSource = Pic.alpha(tranSource, 10);
        tranDesc = Pic.alpha(tranDesc, 100);
        tranAmt = Pic.truncate(tranAmt, 9, 2);
        tranMerchantId = Math.abs(tranMerchantId) % 1_000_000_000L;
        tranMerchantName = Pic.alpha(tranMerchantName, 50);
        tranMerchantCity = Pic.alpha(tranMerchantCity, 50);
        tranMerchantZip = Pic.alpha(tranMerchantZip, 10);
        tranCardNum = Pic.alpha(tranCardNum, 16);
        tranOrigTs = Pic.alpha(tranOrigTs, 26);
        tranProcTs = Pic.alpha(tranProcTs, 26);
        filler = Pic.alpha(filler, 20);
    }

    /** A TRAN-RECORD working-storage area initialised to spaces/zeros. */
    public static TranRecord blank() {
        return new TranRecord("", "", 0, "", "", BigDecimal.ZERO, 0, "", "", "", "", "", "", "");
    }

    public static final RecordCodec<TranRecord> CODEC = new RecordCodec<>() {
        @Override
        public int length() {
            return LENGTH;
        }

        @Override
        public TranRecord decode(String image) {
            Pic.Cursor c = new Pic.Cursor(image, LENGTH);
            return new TranRecord(
                    c.take(16),
                    c.take(2),
                    (int) Pic.parseUnsigned(c.take(4)),
                    c.take(10),
                    c.take(100),
                    Pic.parseSignedZoned(c.take(11), 2),
                    Pic.parseUnsigned(c.take(9)),
                    c.take(50),
                    c.take(50),
                    c.take(10),
                    c.take(16),
                    c.take(26),
                    c.take(26),
                    c.take(20));
        }

        @Override
        public String encode(TranRecord r) {
            return r.tranId
                    + r.tranTypeCd
                    + Pic.unsigned(r.tranCatCd, 4)
                    + r.tranSource
                    + r.tranDesc
                    + Pic.signedZoned(r.tranAmt, 9, 2)
                    + Pic.unsigned(r.tranMerchantId, 9)
                    + r.tranMerchantName
                    + r.tranMerchantCity
                    + r.tranMerchantZip
                    + r.tranCardNum
                    + r.tranOrigTs
                    + r.tranProcTs
                    + r.filler;
        }
    };
}
