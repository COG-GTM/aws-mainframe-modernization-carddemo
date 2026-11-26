package com.carddemo.programs.cbact04c.copybook;

import java.math.BigDecimal;

/**
 * CVTRA01Y - TRAN-CAT-BAL-RECORD (RECLN 50), transaction category balance (TCATBALF KSDS).
 *
 * <pre>
 * 05 TRAN-CAT-KEY.
 *    10 TRANCAT-ACCT-ID   PIC 9(11).
 *    10 TRANCAT-TYPE-CD   PIC X(02).
 *    10 TRANCAT-CD        PIC 9(04).
 * 05 TRAN-CAT-BAL         PIC S9(09)V99.
 * 05 FILLER               PIC X(22).
 * </pre>
 */
public record TranCatBalRecord(
        long trancatAcctId,
        String trancatTypeCd,
        int trancatCd,
        BigDecimal tranCatBal,
        String filler) {

    public static final int LENGTH = 50;

    public TranCatBalRecord {
        trancatTypeCd = Pic.alpha(trancatTypeCd, 2);
        tranCatBal = Pic.truncate(tranCatBal, 9, 2);
        filler = Pic.alpha(filler, 22);
    }

    public TranCatBalRecord(long acctId, String typeCd, int catCd, BigDecimal balance) {
        this(acctId, typeCd, catCd, balance, "");
    }

    /** TRAN-CAT-KEY group. */
    public record Key(long trancatAcctId, String trancatTypeCd, int trancatCd) {
        public Key {
            trancatTypeCd = Pic.alpha(trancatTypeCd, 2);
        }
    }

    public Key key() {
        return new Key(trancatAcctId, trancatTypeCd, trancatCd);
    }

    public static final RecordCodec<TranCatBalRecord> CODEC = new RecordCodec<>() {
        @Override
        public int length() {
            return LENGTH;
        }

        @Override
        public TranCatBalRecord decode(String image) {
            Pic.Cursor c = new Pic.Cursor(image, LENGTH);
            return new TranCatBalRecord(
                    Pic.parseUnsigned(c.take(11)),
                    c.take(2),
                    (int) Pic.parseUnsigned(c.take(4)),
                    Pic.parseSignedZoned(c.take(11), 2),
                    c.take(22));
        }

        @Override
        public String encode(TranCatBalRecord r) {
            return Pic.unsigned(r.trancatAcctId, 11)
                    + r.trancatTypeCd
                    + Pic.unsigned(r.trancatCd, 4)
                    + Pic.signedZoned(r.tranCatBal, 9, 2)
                    + r.filler;
        }
    };
}
