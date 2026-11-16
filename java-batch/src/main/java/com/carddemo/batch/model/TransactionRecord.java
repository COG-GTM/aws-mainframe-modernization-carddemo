package com.carddemo.batch.model;

import com.carddemo.batch.io.FieldWriter;
import com.carddemo.batch.io.FixedWidthCodec;

import java.math.BigDecimal;

/** {@code TRAN-RECORD} from copybook CVTRA05Y (350 bytes), TRANSACT KSDS keyed by {@code TRAN-ID}. */
public record TransactionRecord(
        String tranId,             // PIC X(16)
        String tranTypeCd,         // PIC X(02)
        int tranCatCd,             // PIC 9(04)
        String tranSource,         // PIC X(10)
        String tranDesc,           // PIC X(100)
        BigDecimal tranAmt,        // PIC S9(09)V99
        long tranMerchantId,       // PIC 9(09)
        String tranMerchantName,   // PIC X(50)
        String tranMerchantCity,   // PIC X(50)
        String tranMerchantZip,    // PIC X(10)
        String tranCardNum,        // PIC X(16)
        String tranOrigTs,         // PIC X(26)
        String tranProcTs,         // PIC X(26)
        String filler) {           // PIC X(20)

    public static final int LENGTH = 350;

    public static final FixedWidthCodec<TransactionRecord> CODEC = FixedWidthCodec.of(LENGTH,
            r -> new TransactionRecord(r.text(16), r.text(2), r.unsignedInt(4), r.text(10), r.text(100),
                    r.signedDecimal(11, 2), r.unsignedLong(9), r.text(50), r.text(50), r.text(10), r.text(16),
                    r.text(26), r.text(26), r.text(20)),
            t -> new FieldWriter(LENGTH)
                    .text(t.tranId, 16)
                    .text(t.tranTypeCd, 2)
                    .unsigned(t.tranCatCd, 4)
                    .text(t.tranSource, 10)
                    .text(t.tranDesc, 100)
                    .signedDecimal(t.tranAmt, 11, 2)
                    .unsigned(t.tranMerchantId, 9)
                    .text(t.tranMerchantName, 50)
                    .text(t.tranMerchantCity, 50)
                    .text(t.tranMerchantZip, 10)
                    .text(t.tranCardNum, 16)
                    .text(t.tranOrigTs, 26)
                    .text(t.tranProcTs, 26)
                    .text(t.filler, 20)
                    .toString());
}
