package com.carddemo.data.record;

import com.carddemo.common.codec.CobolField;

import java.math.BigDecimal;

/** Fixed-width TRAN-RECORD. Layout: copybook CVTRA05Y. */
public record TransactionRecord(
        @CobolField("TRAN-ID") String id,
        @CobolField("TRAN-TYPE-CD") String typeCode,
        @CobolField("TRAN-CAT-CD") Integer categoryCode,
        @CobolField("TRAN-SOURCE") String source,
        @CobolField("TRAN-DESC") String description,
        @CobolField("TRAN-AMT") BigDecimal amount,
        @CobolField("TRAN-MERCHANT-ID") Long merchantId,
        @CobolField("TRAN-MERCHANT-NAME") String merchantName,
        @CobolField("TRAN-MERCHANT-CITY") String merchantCity,
        @CobolField("TRAN-MERCHANT-ZIP") String merchantZip,
        @CobolField("TRAN-CARD-NUM") String cardNumber,
        @CobolField("TRAN-ORIG-TS") String originTimestamp,
        @CobolField("TRAN-PROC-TS") String processedTimestamp) {
}
