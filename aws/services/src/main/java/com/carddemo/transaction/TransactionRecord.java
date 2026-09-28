package com.carddemo.transaction;

import java.math.BigDecimal;
import java.time.LocalDateTime;

public record TransactionRecord(
        String tranId,
        String typeCd,
        int catCd,
        String source,
        String description,
        BigDecimal amt,
        Integer merchantId,
        String merchantName,
        String merchantCity,
        String merchantZip,
        String cardNum,
        LocalDateTime origTs,
        LocalDateTime procTs) {
}
