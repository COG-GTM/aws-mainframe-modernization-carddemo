package com.carddemo.programs.cbtrn01c.copybook;

import java.math.BigDecimal;

/** Copybook CVTRA06Y: 01 DALYTRAN-RECORD (RECLN 350). */
public record DalytranRecord(
        String id,              // DALYTRAN-ID            PIC X(16)
        String typeCd,          // DALYTRAN-TYPE-CD       PIC X(02)
        long catCd,             // DALYTRAN-CAT-CD        PIC 9(04)
        String source,          // DALYTRAN-SOURCE        PIC X(10)
        String desc,            // DALYTRAN-DESC          PIC X(100)
        BigDecimal amt,         // DALYTRAN-AMT           PIC S9(09)V99
        long merchantId,        // DALYTRAN-MERCHANT-ID   PIC 9(09)
        String merchantName,    // DALYTRAN-MERCHANT-NAME PIC X(50)
        String merchantCity,    // DALYTRAN-MERCHANT-CITY PIC X(50)
        String merchantZip,     // DALYTRAN-MERCHANT-ZIP  PIC X(10)
        String cardNum,         // DALYTRAN-CARD-NUM      PIC X(16)
        String origTs,          // DALYTRAN-ORIG-TS       PIC X(26)
        String procTs,          // DALYTRAN-PROC-TS       PIC X(26)
        String filler) {        // FILLER                 PIC X(20)

    public static final int LENGTH = 350;

    public DalytranRecord {
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

    /** Working-storage initial state (no VALUE clause): spaces / zeros by PICTURE. */
    public static DalytranRecord initial() {
        return new DalytranRecord("", "", 0, "", "", BigDecimal.ZERO, 0, "", "", "", "", "", "", "");
    }

    public static final RecordCodec<DalytranRecord> CODEC = new RecordCodec<>() {
        @Override
        public int length() {
            return LENGTH;
        }

        @Override
        public DalytranRecord parse(String image) {
            Pic.Reader r = new Pic.Reader(image, LENGTH);
            return new DalytranRecord(r.x(16), r.x(2), r.n(4), r.x(10), r.x(100), r.s(9, 2), r.n(9),
                    r.x(50), r.x(50), r.x(10), r.x(16), r.x(26), r.x(26), r.x(20));
        }

        @Override
        public String format(DalytranRecord d) {
            return d.id + d.typeCd + Pic.formatUnsigned(d.catCd, 4) + d.source + d.desc
                    + Pic.formatSigned(d.amt, 9, 2) + Pic.formatUnsigned(d.merchantId, 9)
                    + d.merchantName + d.merchantCity + d.merchantZip + d.cardNum + d.origTs + d.procTs
                    + d.filler;
        }
    };

    public String toFixedWidth() {
        return CODEC.format(this);
    }
}
