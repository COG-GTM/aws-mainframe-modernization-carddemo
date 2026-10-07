package com.carddemo.user;

import com.carddemo.common.data.CodedEnumConverter;
import jakarta.persistence.Converter;

/** Stores {@link UserType} as its COBOL code. */
@Converter(autoApply = true)
public class UserTypeConverter extends CodedEnumConverter<UserType> {

    public UserTypeConverter() {
        super(UserType.class);
    }
}
