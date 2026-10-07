package com.carddemo.common.data;

import jakarta.persistence.AttributeConverter;

/** Persists a {@link CodedEnum} as its COBOL code, never as ordinal or name (ADR-0006). */
public abstract class CodedEnumConverter<E extends Enum<E> & CodedEnum> implements AttributeConverter<E, String> {

    private final Class<E> type;

    protected CodedEnumConverter(Class<E> type) {
        this.type = type;
    }

    @Override
    public String convertToDatabaseColumn(E value) {
        return value == null ? null : value.code();
    }

    @Override
    public E convertToEntityAttribute(String code) {
        return code == null ? null : CodedEnum.fromCode(type, code);
    }
}
