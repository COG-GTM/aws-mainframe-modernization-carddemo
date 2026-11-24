package com.carddemo.transaction;

import com.carddemo.common.domain.CardTransaction;
import java.math.BigDecimal;
import java.time.LocalDateTime;

public record TransactionView(String tranId, String typeCd, Integer catCd, String source, String description,
                              BigDecimal amount, Long merchantId, String merchantName, String merchantCity,
                              String merchantZip, String cardNum, LocalDateTime origTs, LocalDateTime procTs) {

    static TransactionView of(CardTransaction t) {
        return new TransactionView(t.getTranId(), t.getTypeCd(), t.getCatCd(), t.getSource(), t.getDescription(),
                t.getAmount(), t.getMerchantId(), t.getMerchantName(), t.getMerchantCity(), t.getMerchantZip(),
                t.getCardNum(), t.getOrigTs(), t.getProcTs());
    }
}
