package com.carddemo.transaction.api.dto;

import com.carddemo.common.api.PicId;
import com.carddemo.transaction.domain.Transaction;
import java.math.BigDecimal;
import java.time.LocalDateTime;

/** Screen fields of COTRN01 (view) and the list rows of COTRN00. */
public record TransactionResponse(
        String transactionId,
        String cardNumber,
        String typeCode,
        Integer categoryCode,
        String source,
        String description,
        BigDecimal amount,
        @PicId(digits = 9) Long merchantId,
        String merchantName,
        String merchantCity,
        String merchantZip,
        LocalDateTime originTimestamp,
        LocalDateTime processTimestamp) {

    public static TransactionResponse from(Transaction transaction) {
        return new TransactionResponse(
                transaction.getTranId(),
                transaction.getCardNum(),
                transaction.getTypeCd(),
                transaction.getCatCd(),
                transaction.getSource(),
                transaction.getDescription(),
                transaction.getAmount(),
                transaction.getMerchantId(),
                transaction.getMerchantName(),
                transaction.getMerchantCity(),
                transaction.getMerchantZip(),
                transaction.getOrigTs(),
                transaction.getProcTs());
    }
}
