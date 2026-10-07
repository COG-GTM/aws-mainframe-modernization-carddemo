package com.carddemo.card.online;

import com.carddemo.card.Card;
import com.carddemo.common.online.ScreenInput;

/**
 * {@code CCUP-NEW-CARDDATA} / {@code CCUP-OLD-CARDDATA}: the editable fields of map CCRDUPA as typed (or as read).
 *
 * @param embossedName {@code CRDNAME} X(50)
 * @param activeStatus {@code CRDSTCD} X(1)
 * @param expiryMonth  {@code EXPMON} X(2)
 * @param expiryYear   {@code EXPYEAR} X(4)
 */
public record CardChanges(String embossedName, String activeStatus, String expiryMonth, String expiryYear) {

    /**
     * {@code 9100-GETCARD-BYACCTCARD} → {@code CCUP-OLD-*}: the embossed name upper-cased, expiry year
     * {@code (1:4)} and month {@code (6:2)} of {@code CARD-EXPIRAION-DATE} (COCRDUPC R-25).
     */
    public static CardChanges fetched(Card card) {
        String expiry = card.getExpirationDate();
        return new CardChanges(ScreenInput.upperCase(ScreenInput.rightTrim(card.getEmbossedName())),
                card.getActiveStatus().code(), expiry.substring(5, 7), expiry.substring(0, 4));
    }

    /** {@code CCUP-OLD-EXPDAY}: {@code CARD-EXPIRAION-DATE(9:2)}, kept on rewrite (the day is not on the map). */
    public static String expiryDay(Card card) {
        return card.getExpirationDate().substring(8, 10);
    }
}
