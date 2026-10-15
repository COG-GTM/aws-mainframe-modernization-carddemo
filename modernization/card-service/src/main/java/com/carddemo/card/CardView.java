package com.carddemo.card;

import com.carddemo.common.domain.Card;
import java.time.LocalDate;

/** CCRDLIA / CCRDSLA / CCRDUPA fields. CVV is never returned. */
public record CardView(String cardNum, Long acctId, String embossedName, LocalDate expirationDate,
                       String activeStatus, long version) {

    static CardView of(Card c) {
        return new CardView(c.getCardNum(), c.getAcctId(), c.getEmbossedName(), c.getExpirationDate(),
                c.getActiveStatus(), c.getVersion());
    }
}
