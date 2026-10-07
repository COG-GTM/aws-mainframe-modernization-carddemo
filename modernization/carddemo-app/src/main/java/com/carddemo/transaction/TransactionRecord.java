package com.carddemo.transaction;

import com.carddemo.common.data.CobolField;
import com.carddemo.common.data.CopybookRecordMapper;
import java.math.BigDecimal;

/**
 * One TRANSACT record (copybook CVTRA05Y) as an immutable value. {@link #MAPPER} converts it to and from
 * the fixed-width record; {@link Transaction} persists it.
 *
 * @param tranId TRAN-ID PIC X(16)
 * @param tranTypeCd TRAN-TYPE-CD PIC X(02)
 * @param tranCatCd TRAN-CAT-CD PIC 9(04)
 * @param source TRAN-SOURCE PIC X(10)
 * @param description TRAN-DESC PIC X(100)
 * @param amount TRAN-AMT PIC S9(09)V99
 * @param merchantId TRAN-MERCHANT-ID PIC 9(09)
 * @param merchantName TRAN-MERCHANT-NAME PIC X(50)
 * @param merchantCity TRAN-MERCHANT-CITY PIC X(50)
 * @param merchantZip TRAN-MERCHANT-ZIP PIC X(10)
 * @param cardNum TRAN-CARD-NUM PIC X(16)
 * @param origTs TRAN-ORIG-TS PIC X(26)
 * @param procTs TRAN-PROC-TS PIC X(26)
 */
public record TransactionRecord(
        @CobolField("TRAN-ID") String tranId,
        @CobolField("TRAN-TYPE-CD") String tranTypeCd,
        @CobolField("TRAN-CAT-CD") int tranCatCd,
        @CobolField("TRAN-SOURCE") String source,
        @CobolField("TRAN-DESC") String description,
        @CobolField("TRAN-AMT") BigDecimal amount,
        @CobolField("TRAN-MERCHANT-ID") int merchantId,
        @CobolField("TRAN-MERCHANT-NAME") String merchantName,
        @CobolField("TRAN-MERCHANT-CITY") String merchantCity,
        @CobolField("TRAN-MERCHANT-ZIP") String merchantZip,
        @CobolField("TRAN-CARD-NUM") String cardNum,
        @CobolField("TRAN-ORIG-TS") String origTs,
        @CobolField("TRAN-PROC-TS") String procTs) {

    public static final CopybookRecordMapper<TransactionRecord> MAPPER =
            CopybookRecordMapper.of(TransactionRecord.class, "CVTRA05Y");
}
