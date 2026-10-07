package com.carddemo.account;

import com.carddemo.common.data.CodedEnumConverter;
import jakarta.persistence.Converter;

/** Stores {@link AccountStatus} as its COBOL code. */
@Converter(autoApply = true)
public class AccountStatusConverter extends CodedEnumConverter<AccountStatus> {

    public AccountStatusConverter() {
        super(AccountStatus.class);
    }
}
