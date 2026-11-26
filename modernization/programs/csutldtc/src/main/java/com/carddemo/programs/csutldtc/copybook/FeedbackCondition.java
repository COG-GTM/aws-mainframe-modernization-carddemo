package com.carddemo.programs.csutldtc.copybook;

import java.util.Arrays;
import java.util.HexFormat;
import java.util.Optional;

/** 88-level condition names on FEEDBACK-TOKEN-VALUE (8 bytes) in CSUTLDTC. */
public enum FeedbackCondition {
    /** 88 FC-INVALID-DATE (despite the name, all-zero token = CEE000 success). */
    FC_INVALID_DATE("0000000000000000"),
    FC_INSUFFICIENT_DATA("000309CB59C3C5C5"),
    FC_BAD_DATE_VALUE("000309CC59C3C5C5"),
    FC_INVALID_ERA("000309CD59C3C5C5"),
    FC_UNSUPP_RANGE("000309D159C3C5C5"),
    FC_INVALID_MONTH("000309D559C3C5C5"),
    FC_BAD_PIC_STRING("000309D659C3C5C5"),
    FC_NON_NUMERIC_DATA("000309D859C3C5C5"),
    FC_YEAR_IN_ERA_ZERO("000309D959C3C5C5");

    private final byte[] value;

    FeedbackCondition(String hex) {
        this.value = HexFormat.of().parseHex(hex);
    }

    public byte[] value() {
        return value.clone();
    }

    public boolean matches(FeedbackCode code) {
        return Arrays.equals(value, code.tokenValue());
    }

    public static Optional<FeedbackCondition> of(FeedbackCode code) {
        return Arrays.stream(values()).filter(c -> c.matches(code)).findFirst();
    }
}
