package com.carddemo.card.api.dto;

import com.carddemo.common.api.PicId;
import com.carddemo.card.domain.CardXref;

public record CardXrefResponse(String cardNumber, @PicId(digits = 9) Long customerId,
                               @PicId(digits = 11) Long accountId) {

    public static CardXrefResponse from(CardXref xref) {
        return new CardXrefResponse(xref.getCardNum(), xref.getCustId(), xref.getAcctId());
    }
}
