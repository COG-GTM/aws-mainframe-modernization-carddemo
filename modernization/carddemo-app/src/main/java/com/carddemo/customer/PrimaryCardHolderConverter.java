package com.carddemo.customer;

import com.carddemo.common.data.CodedEnumConverter;
import jakarta.persistence.Converter;

/** Stores {@link PrimaryCardHolder} as its COBOL code. */
@Converter(autoApply = true)
public class PrimaryCardHolderConverter extends CodedEnumConverter<PrimaryCardHolder> {

    public PrimaryCardHolderConverter() {
        super(PrimaryCardHolder.class);
    }
}
