package com.carddemo.data.entity;

import jakarta.persistence.AttributeConverter;

import java.util.Arrays;

/** SEC-USR-TYPE / CDEMO-USER-TYPE: 88 CDEMO-USRTYP-ADMIN VALUE 'A', 88 CDEMO-USRTYP-USER VALUE 'U'. */
public enum UserType {
    ADMIN("A"),
    USER("U");

    private final String code;

    UserType(String code) {
        this.code = code;
    }

    public String code() {
        return code;
    }

    public static UserType fromCode(String code) {
        if (code == null || code.isBlank()) {
            return null;
        }
        return Arrays.stream(values()).filter(t -> t.code.equals(code)).findFirst()
                .orElseThrow(() -> new IllegalArgumentException("Unknown SEC-USR-TYPE '" + code + "'"));
    }

    /** Persists the one-character COBOL code. */
    @jakarta.persistence.Converter
    public static class Converter implements AttributeConverter<UserType, String> {

        @Override
        public String convertToDatabaseColumn(UserType type) {
            return type == null ? null : type.code;
        }

        @Override
        public UserType convertToEntityAttribute(String code) {
            return fromCode(code);
        }
    }
}
