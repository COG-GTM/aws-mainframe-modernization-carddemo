package com.carddemo.transaction;

import com.carddemo.common.data.CobolField;
import com.carddemo.common.data.CopybookRecordMapper;
import java.math.BigDecimal;

/**
 * One DALYTRAN record (copybook CVTRA06Y) as an immutable value. {@link #MAPPER} converts it to and from
 * the fixed-width record; {@link DailyTransaction} persists it.
 *
 * @param tranId DALYTRAN-ID PIC X(16)
 * @param tranTypeCd DALYTRAN-TYPE-CD PIC X(02)
 * @param tranCatCd DALYTRAN-CAT-CD PIC 9(04)
 * @param source DALYTRAN-SOURCE PIC X(10)
 * @param description DALYTRAN-DESC PIC X(100)
 * @param amount DALYTRAN-AMT PIC S9(09)V99
 * @param merchantId DALYTRAN-MERCHANT-ID PIC 9(09)
 * @param merchantName DALYTRAN-MERCHANT-NAME PIC X(50)
 * @param merchantCity DALYTRAN-MERCHANT-CITY PIC X(50)
 * @param merchantZip DALYTRAN-MERCHANT-ZIP PIC X(10)
 * @param cardNum DALYTRAN-CARD-NUM PIC X(16)
 * @param origTs DALYTRAN-ORIG-TS PIC X(26)
 * @param procTs DALYTRAN-PROC-TS PIC X(26)
 */
public record DailyTransactionRecord(
        @CobolField("DALYTRAN-ID") String tranId,
        @CobolField("DALYTRAN-TYPE-CD") String tranTypeCd,
        @CobolField("DALYTRAN-CAT-CD") int tranCatCd,
        @CobolField("DALYTRAN-SOURCE") String source,
        @CobolField("DALYTRAN-DESC") String description,
        @CobolField("DALYTRAN-AMT") BigDecimal amount,
        @CobolField("DALYTRAN-MERCHANT-ID") int merchantId,
        @CobolField("DALYTRAN-MERCHANT-NAME") String merchantName,
        @CobolField("DALYTRAN-MERCHANT-CITY") String merchantCity,
        @CobolField("DALYTRAN-MERCHANT-ZIP") String merchantZip,
        @CobolField("DALYTRAN-CARD-NUM") String cardNum,
        @CobolField("DALYTRAN-ORIG-TS") String origTs,
        @CobolField("DALYTRAN-PROC-TS") String procTs) {

    public static final CopybookRecordMapper<DailyTransactionRecord> MAPPER =
            CopybookRecordMapper.of(DailyTransactionRecord.class, "CVTRA06Y");

    /** CBTRN02C 2000-POST-TRANSACTION: {@code MOVE DALYTRAN-* TO TRAN-*}, field for field. */
    public TransactionRecord asTransaction() {
        return new TransactionRecord(tranId, tranTypeCd, tranCatCd, source, description, amount, merchantId,
                merchantName, merchantCity, merchantZip, cardNum, origTs, procTs);
    }
}
