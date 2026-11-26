package com.carddemo.programs.cbact04c.copybook;

import java.math.BigDecimal;

/**
 * CVTRA02Y - DIS-GROUP-RECORD (RECLN 50), disclosure group / interest rate (DISCGRP KSDS).
 *
 * <pre>
 * 05 DIS-GROUP-KEY.
 *    10 DIS-ACCT-GROUP-ID   PIC X(10).
 *    10 DIS-TRAN-TYPE-CD    PIC X(02).
 *    10 DIS-TRAN-CAT-CD     PIC 9(04).
 * 05 DIS-INT-RATE           PIC S9(04)V99.
 * 05 FILLER                 PIC X(28).
 * </pre>
 */
public record DisGroupRecord(
        String disAcctGroupId,
        String disTranTypeCd,
        int disTranCatCd,
        BigDecimal disIntRate,
        String filler) {

    public static final int LENGTH = 50;

    public DisGroupRecord {
        disAcctGroupId = Pic.alpha(disAcctGroupId, 10);
        disTranTypeCd = Pic.alpha(disTranTypeCd, 2);
        disIntRate = Pic.truncate(disIntRate, 4, 2);
        filler = Pic.alpha(filler, 28);
    }

    public DisGroupRecord(String groupId, String typeCd, int catCd, BigDecimal rate) {
        this(groupId, typeCd, catCd, rate, "");
    }

    /** DIS-GROUP-KEY group (also the FD-DISCGRP-KEY record key). */
    public record Key(String disAcctGroupId, String disTranTypeCd, int disTranCatCd) {
        public Key {
            disAcctGroupId = Pic.alpha(disAcctGroupId, 10);
            disTranTypeCd = Pic.alpha(disTranTypeCd, 2);
            disTranCatCd = (int) (Math.abs((long) disTranCatCd) % 10_000);
        }
    }

    public Key key() {
        return new Key(disAcctGroupId, disTranTypeCd, disTranCatCd);
    }

    public static final RecordCodec<DisGroupRecord> CODEC = new RecordCodec<>() {
        @Override
        public int length() {
            return LENGTH;
        }

        @Override
        public DisGroupRecord decode(String image) {
            Pic.Cursor c = new Pic.Cursor(image, LENGTH);
            return new DisGroupRecord(
                    c.take(10),
                    c.take(2),
                    (int) Pic.parseUnsigned(c.take(4)),
                    Pic.parseSignedZoned(c.take(6), 2),
                    c.take(28));
        }

        @Override
        public String encode(DisGroupRecord r) {
            return r.disAcctGroupId
                    + r.disTranTypeCd
                    + Pic.unsigned(r.disTranCatCd, 4)
                    + Pic.signedZoned(r.disIntRate, 4, 2)
                    + r.filler;
        }
    };
}
