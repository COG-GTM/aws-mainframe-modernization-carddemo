package com.carddemo.batch.model;

import com.carddemo.batch.io.FieldReader;
import com.carddemo.batch.io.FieldWriter;
import com.carddemo.batch.io.FixedWidthCodec;

import java.math.BigDecimal;

/** {@code TRAN-CAT-BAL-RECORD} from copybook CVTRA01Y (50 bytes), TCATBALF KSDS keyed by {@code TRAN-CAT-KEY}. */
public record TranCatBalRecord(
        TranCatKey tranCatKey,
        BigDecimal tranCatBal,   // PIC S9(09)V99
        String filler) {         // PIC X(22)

    public static final int LENGTH = 50;

    /** {@code TRAN-CAT-KEY} (17 bytes); ordered like the VSAM key bytes. */
    public record TranCatKey(
            long trancatAcctId,     // PIC 9(11)
            String trancatTypeCd,   // PIC X(02)
            int trancatCd)          // PIC 9(04)
            implements Comparable<TranCatKey> {

        public static final int LENGTH = 17;

        static TranCatKey read(FieldReader r) {
            return new TranCatKey(r.unsignedLong(11), r.text(2), r.unsignedInt(4));
        }

        FieldWriter write(FieldWriter w) {
            return w.unsigned(trancatAcctId, 11).text(trancatTypeCd, 2).unsigned(trancatCd, 4);
        }

        public String encode() {
            return write(new FieldWriter(LENGTH)).toString();
        }

        @Override
        public int compareTo(TranCatKey o) {
            return encode().compareTo(o.encode());
        }
    }

    public static final FixedWidthCodec<TranCatBalRecord> CODEC = FixedWidthCodec.of(LENGTH,
            r -> new TranCatBalRecord(TranCatKey.read(r), r.signedDecimal(11, 2), r.text(22)),
            t -> t.tranCatKey.write(new FieldWriter(LENGTH))
                    .signedDecimal(t.tranCatBal, 11, 2)
                    .text(t.filler, 22)
                    .toString());

    public TranCatBalRecord withTranCatBal(BigDecimal v) {
        return new TranCatBalRecord(tranCatKey, v, filler);
    }
}
