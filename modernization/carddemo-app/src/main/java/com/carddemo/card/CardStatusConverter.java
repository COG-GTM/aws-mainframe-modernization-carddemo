package com.carddemo.card;

import com.carddemo.common.data.CodedEnumConverter;
import jakarta.persistence.Converter;

/** Stores {@link CardStatus} as its COBOL code. */
@Converter(autoApply = true)
public class CardStatusConverter extends CodedEnumConverter<CardStatus> {

    public CardStatusConverter() {
        super(CardStatus.class);
    }
}
