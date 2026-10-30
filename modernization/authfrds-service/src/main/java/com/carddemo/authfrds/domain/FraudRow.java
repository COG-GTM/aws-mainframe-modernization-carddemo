package com.carddemo.authfrds.domain;

import java.math.BigDecimal;

/** DCLGEN DCLAUTHFRDS host variables as COPAUS2C fills them (AUTH-TS is the X(26) character form). */
public record FraudRow(
        String cardNum, String authTsHost, String authType, String cardExpiryDate, String messageType,
        String messageSource, String authIdCode, String authRespCode, String authRespReason, String processingCode,
        BigDecimal transactionAmt, BigDecimal approvedAmt, String merchantCategoryCode, String acqrCountryCode,
        int posEntryMode, String merchantId, String merchantName, String merchantCity, String merchantState,
        String merchantZip, String transactionId, String matchStatus, String authFraud, long acctId, long custId) {
}
