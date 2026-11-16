package com.carddemo.batch.model;

import com.carddemo.batch.io.FieldWriter;
import com.carddemo.batch.io.FixedWidthCodec;

import java.math.BigDecimal;

/** {@code DALYTRAN-RECORD} from copybook CVTRA06Y (350 bytes), the sequential DALYTRAN input to POSTTRAN. */
public record DailyTransactionRecord(
        String dalytranId,             // PIC X(16)
        String dalytranTypeCd,         // PIC X(02)
        int dalytranCatCd,             // PIC 9(04)
        String dalytranSource,         // PIC X(10)
        String dalytranDesc,           // PIC X(100)
        BigDecimal dalytranAmt,        // PIC S9(09)V99
        long dalytranMerchantId,       // PIC 9(09)
        String dalytranMerchantName,   // PIC X(50)
        String dalytranMerchantCity,   // PIC X(50)
        String dalytranMerchantZip,    // PIC X(10)
        String dalytranCardNum,        // PIC X(16)
        String dalytranOrigTs,         // PIC X(26)
        String dalytranProcTs,         // PIC X(26)
        String filler) {               // PIC X(20)

    public static final int LENGTH = 350;

    public static final FixedWidthCodec<DailyTransactionRecord> CODEC = FixedWidthCodec.of(LENGTH,
            r -> new DailyTransactionRecord(r.text(16), r.text(2), r.unsignedInt(4), r.text(10), r.text(100),
                    r.signedDecimal(11, 2), r.unsignedLong(9), r.text(50), r.text(50), r.text(10), r.text(16),
                    r.text(26), r.text(26), r.text(20)),
            t -> new FieldWriter(LENGTH)
                    .text(t.dalytranId, 16)
                    .text(t.dalytranTypeCd, 2)
                    .unsigned(t.dalytranCatCd, 4)
                    .text(t.dalytranSource, 10)
                    .text(t.dalytranDesc, 100)
                    .signedDecimal(t.dalytranAmt, 11, 2)
                    .unsigned(t.dalytranMerchantId, 9)
                    .text(t.dalytranMerchantName, 50)
                    .text(t.dalytranMerchantCity, 50)
                    .text(t.dalytranMerchantZip, 10)
                    .text(t.dalytranCardNum, 16)
                    .text(t.dalytranOrigTs, 26)
                    .text(t.dalytranProcTs, 26)
                    .text(t.filler, 20)
                    .toString());
}
