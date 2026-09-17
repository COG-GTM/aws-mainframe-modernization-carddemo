package com.carddemo.card.api.dto;

import com.carddemo.card.domain.Card;
import java.time.LocalDate;

/** Screen fields of COCRDSL / COCRDLI. The CVV is never returned; it left the mainframe screen too. */
public record CardResponse(
        String cardNumber,
        Long accountId,
        String embossedName,
        LocalDate expirationDate,
        String activeStatus) {

    public static CardResponse from(Card card) {
        return new CardResponse(card.getCardNum(), card.getAcctId(), card.getEmbossedName(),
                card.getExpirationDate(), card.getActiveStatus());
    }
}
