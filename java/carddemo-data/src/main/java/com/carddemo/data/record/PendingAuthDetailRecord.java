package com.carddemo.data.record;

import com.carddemo.common.codec.CobolField;

import java.math.BigDecimal;

/** IMS segment PAUTDTL1 (COMP-3 key and amounts); the parent account id is not part of the segment. Layout: copybook CIPAUDTY. */
public record PendingAuthDetailRecord(
        @CobolField("PA-AUTHORIZATION-KEY") Key key,
        @CobolField("PA-AUTH-ORIG-DATE") String authOrigDate,
        @CobolField("PA-AUTH-ORIG-TIME") String authOrigTime,
        @CobolField("PA-CARD-NUM") String cardNumber,
        @CobolField("PA-AUTH-TYPE") String authType,
        @CobolField("PA-CARD-EXPIRY-DATE") String cardExpiryDate,
        @CobolField("PA-MESSAGE-TYPE") String messageType,
        @CobolField("PA-MESSAGE-SOURCE") String messageSource,
        @CobolField("PA-AUTH-ID-CODE") String authIdCode,
        @CobolField("PA-AUTH-RESP-CODE") String authRespCode,
        @CobolField("PA-AUTH-RESP-REASON") String authRespReason,
        @CobolField("PA-PROCESSING-CODE") Integer processingCode,
        @CobolField("PA-TRANSACTION-AMT") BigDecimal transactionAmount,
        @CobolField("PA-APPROVED-AMT") BigDecimal approvedAmount,
        @CobolField("PA-MERCHANT-CATAGORY-CODE") String merchantCategoryCode,
        @CobolField("PA-ACQR-COUNTRY-CODE") String acquirerCountryCode,
        @CobolField("PA-POS-ENTRY-MODE") Integer posEntryMode,
        @CobolField("PA-MERCHANT-ID") String merchantId,
        @CobolField("PA-MERCHANT-NAME") String merchantName,
        @CobolField("PA-MERCHANT-CITY") String merchantCity,
        @CobolField("PA-MERCHANT-STATE") String merchantState,
        @CobolField("PA-MERCHANT-ZIP") String merchantZip,
        @CobolField("PA-TRANSACTION-ID") String transactionId,
        @CobolField("PA-MATCH-STATUS") String matchStatus,
        @CobolField("PA-AUTH-FRAUD") String authFraud,
        @CobolField("PA-FRAUD-RPT-DATE") String fraudReportDate) {

    /** PA-AUTHORIZATION-KEY: inverted date/time (99999-YYDDD, 999999999-HHMMSSmmm). */
    public record Key(
            @CobolField("PA-AUTH-DATE-9C") Integer authDate,
            @CobolField("PA-AUTH-TIME-9C") Long authTime) {
    }
}
