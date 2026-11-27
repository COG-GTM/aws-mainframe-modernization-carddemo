package com.carddemo.data.record;

import com.carddemo.common.codec.CobolField;

import java.math.BigDecimal;

/** Fixed-width DALYTRAN-RECORD. Layout: copybook CVTRA06Y. */
public record DailyTransactionRecord(
        @CobolField("DALYTRAN-ID") String id,
        @CobolField("DALYTRAN-TYPE-CD") String typeCode,
        @CobolField("DALYTRAN-CAT-CD") Integer categoryCode,
        @CobolField("DALYTRAN-SOURCE") String source,
        @CobolField("DALYTRAN-DESC") String description,
        @CobolField("DALYTRAN-AMT") BigDecimal amount,
        @CobolField("DALYTRAN-MERCHANT-ID") Long merchantId,
        @CobolField("DALYTRAN-MERCHANT-NAME") String merchantName,
        @CobolField("DALYTRAN-MERCHANT-CITY") String merchantCity,
        @CobolField("DALYTRAN-MERCHANT-ZIP") String merchantZip,
        @CobolField("DALYTRAN-CARD-NUM") String cardNumber,
        @CobolField("DALYTRAN-ORIG-TS") String originTimestamp,
        @CobolField("DALYTRAN-PROC-TS") String processedTimestamp) {
}
