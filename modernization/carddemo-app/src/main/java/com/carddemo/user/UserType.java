package com.carddemo.user;

import com.carddemo.common.data.CodedEnum;

/** SEC-USR-TYPE: COCOM01Y {@code 88 CDEMO-USRTYP-ADMIN VALUE 'A'} / {@code 88 CDEMO-USRTYP-USER VALUE 'U'}. */
public enum UserType implements CodedEnum {
    /** CDEMO-USRTYP-ADMIN. */
    ADMIN("A"),
    /** CDEMO-USRTYP-USER. */
    USER("U");

    private final String code;

    UserType(String code) {
        this.code = code;
    }

    @Override
    public String code() {
        return code;
    }

    public static UserType fromCode(String code) {
        return CodedEnum.fromCode(UserType.class, code);
    }
}
