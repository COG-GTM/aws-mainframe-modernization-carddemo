package com.carddemo.card.api.dto;

import com.carddemo.card.domain.CardXref;

public record CardXrefResponse(String cardNumber, Long customerId, Long accountId) {

    public static CardXrefResponse from(CardXref xref) {
        return new CardXrefResponse(xref.getCardNum(), xref.getCustId(), xref.getAcctId());
    }
}
