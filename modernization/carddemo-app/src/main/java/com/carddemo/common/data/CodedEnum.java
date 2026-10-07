package com.carddemo.common.data;

import com.carddemo.common.InvalidRequestException;

/** A level-88 condition value set (ADR-0006): each constant carries the COBOL code that is stored and displayed. */
public interface CodedEnum {

    String code();

    /** The constant for {@code code}; codes outside the 88 set are rejected with {@link InvalidRequestException}. */
    static <E extends Enum<E> & CodedEnum> E fromCode(Class<E> type, String code) {
        for (E constant : type.getEnumConstants()) {
            if (constant.code().equals(code)) {
                return constant;
            }
        }
        throw new InvalidRequestException(type.getSimpleName() + ": undefined code '" + code + "'");
    }
}
