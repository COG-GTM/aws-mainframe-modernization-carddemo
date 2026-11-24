package com.carddemo.authfrds.domain;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

/** One CARDDEMO.AUTHFRDS row as stored in Db2. */
public record FraudRecord(
        String cardNum, LocalDateTime authTs, String authType, String cardExpiryDate, String messageType,
        String messageSource, String authIdCode, String authRespCode, String authRespReason, String processingCode,
        BigDecimal transactionAmt, BigDecimal approvedAmt, String merchantCategoryCode, String acqrCountryCode,
        Integer posEntryMode, String merchantId, String merchantName, String merchantCity, String merchantState,
        String merchantZip, String transactionId, String matchStatus, String authFraud, LocalDate fraudRptDate,
        Long acctId, Long custId) {
}
