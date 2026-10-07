package com.carddemo.customer;

import com.carddemo.common.data.CodedEnum;

/** CUST-PRI-CARD-HOLDER-IND: COACTUPC {@code 88 FLG-PRI-CARDHOLDER-ISVALID VALUES 'Y', 'N'}. */
public enum PrimaryCardHolder implements CodedEnum {
    YES("Y"),
    NO("N");

    private final String code;

    PrimaryCardHolder(String code) {
        this.code = code;
    }

    @Override
    public String code() {
        return code;
    }

    public static PrimaryCardHolder fromCode(String code) {
        return CodedEnum.fromCode(PrimaryCardHolder.class, code);
    }
}
