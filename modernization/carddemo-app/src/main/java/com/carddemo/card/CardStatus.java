package com.carddemo.card;

import com.carddemo.common.data.CodedEnum;

/** CARD-ACTIVE-STATUS: COCRDUPC card status edit, {@code 'Y'} or {@code 'N'}. */
public enum CardStatus implements CodedEnum {
    ACTIVE("Y"),
    INACTIVE("N");

    private final String code;

    CardStatus(String code) {
        this.code = code;
    }

    @Override
    public String code() {
        return code;
    }

    public static CardStatus fromCode(String code) {
        return CodedEnum.fromCode(CardStatus.class, code);
    }
}
