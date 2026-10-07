package com.carddemo.account;

import com.carddemo.common.data.CodedEnum;

/** ACCT-ACTIVE-STATUS: COACTUPC {@code 88 FLG-ACCT-STATUS-ISVALID VALUES 'Y', 'N'}. */
public enum AccountStatus implements CodedEnum {
    ACTIVE("Y"),
    INACTIVE("N");

    private final String code;

    AccountStatus(String code) {
        this.code = code;
    }

    @Override
    public String code() {
        return code;
    }

    public static AccountStatus fromCode(String code) {
        return CodedEnum.fromCode(AccountStatus.class, code);
    }
}
