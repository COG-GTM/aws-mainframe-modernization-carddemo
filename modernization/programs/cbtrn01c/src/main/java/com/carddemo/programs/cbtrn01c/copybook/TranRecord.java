package com.carddemo.programs.cbtrn01c.copybook;

import java.math.BigDecimal;

/** Copybook CVTRA05Y: 01 TRAN-RECORD (RECLN 350). */
public record TranRecord(
        String id,              // TRAN-ID            PIC X(16)
        String typeCd,          // TRAN-TYPE-CD       PIC X(02)
        long catCd,             // TRAN-CAT-CD        PIC 9(04)
        String source,          // TRAN-SOURCE        PIC X(10)
        String desc,            // TRAN-DESC          PIC X(100)
        BigDecimal amt,         // TRAN-AMT           PIC S9(09)V99
        long merchantId,        // TRAN-MERCHANT-ID   PIC 9(09)
        String merchantName,    // TRAN-MERCHANT-NAME PIC X(50)
        String merchantCity,    // TRAN-MERCHANT-CITY PIC X(50)
        String merchantZip,     // TRAN-MERCHANT-ZIP  PIC X(10)
        String cardNum,         // TRAN-CARD-NUM      PIC X(16)
        String origTs,          // TRAN-ORIG-TS       PIC X(26)
        String procTs,          // TRAN-PROC-TS       PIC X(26)
        String filler) {        // FILLER             PIC X(20)

    public static final int LENGTH = 350;

    public TranRecord {
        id = Pic.alpha(id, 16);
        typeCd = Pic.alpha(typeCd, 2);
        catCd = Pic.unsigned(catCd, 4);
        source = Pic.alpha(source, 10);
        desc = Pic.alpha(desc, 100);
        amt = Pic.signed(amt, 9, 2);
        merchantId = Pic.unsigned(merchantId, 9);
        merchantName = Pic.alpha(merchantName, 50);
        merchantCity = Pic.alpha(merchantCity, 50);
        merchantZip = Pic.alpha(merchantZip, 10);
        cardNum = Pic.alpha(cardNum, 16);
        origTs = Pic.alpha(origTs, 26);
        procTs = Pic.alpha(procTs, 26);
        filler = Pic.alpha(filler, 20);
    }

    public static TranRecord initial() {
        return new TranRecord("", "", 0, "", "", BigDecimal.ZERO, 0, "", "", "", "", "", "", "");
    }

    public static final RecordCodec<TranRecord> CODEC = new RecordCodec<>() {
        @Override
        public int length() {
            return LENGTH;
        }

        @Override
        public TranRecord parse(String image) {
            Pic.Reader r = new Pic.Reader(image, LENGTH);
            return new TranRecord(r.x(16), r.x(2), r.n(4), r.x(10), r.x(100), r.s(9, 2), r.n(9),
                    r.x(50), r.x(50), r.x(10), r.x(16), r.x(26), r.x(26), r.x(20));
        }

        @Override
        public String format(TranRecord t) {
            return t.id + t.typeCd + Pic.formatUnsigned(t.catCd, 4) + t.source + t.desc
                    + Pic.formatSigned(t.amt, 9, 2) + Pic.formatUnsigned(t.merchantId, 9)
                    + t.merchantName + t.merchantCity + t.merchantZip + t.cardNum + t.origTs + t.procTs
                    + t.filler;
        }
    };

    public String toFixedWidth() {
        return CODEC.format(this);
    }
}
