package com.carddemo.domain.date;

import java.util.Arrays;
import java.util.HexFormat;

/**
 * The {@code CEEDAYS} feedback-code condition tokens {@code app/cbl/CSUTLDTC.cbl} tests (its
 * {@code FC-*} 88-levels on {@code FEEDBACK-TOKEN-VALUE}), each with the {@code WS-RESULT} text its
 * {@code EVALUATE} moves. The text is kept at the {@code PIC X(15)} width CSUTLDTC writes.
 */
public enum CeedaysFeedback {

    /** {@code FC-INVALID-DATE}: despite its name, the all-zero token CEEDAYS returns on success. */
    VALID("0000000000000000", "Date is valid  "),
    /** {@code FC-INSUFFICIENT-DATA}, CEE2EB. */
    INSUFFICIENT_DATA("000309CB59C3C5C5", "Insufficient   "),
    /** {@code FC-BAD-DATE-VALUE}, CEE2EC. */
    BAD_DATE_VALUE("000309CC59C3C5C5", "Datevalue error"),
    /** {@code FC-INVALID-ERA}, CEE2ED. */
    INVALID_ERA("000309CD59C3C5C5", "Invalid Era    "),
    /** {@code FC-UNSUPP-RANGE}, CEE2F1. */
    UNSUPPORTED_RANGE("000309D159C3C5C5", "Unsupp. Range  "),
    /** {@code FC-INVALID-MONTH}, CEE2F5. */
    INVALID_MONTH("000309D559C3C5C5", "Invalid month  "),
    /** {@code FC-BAD-PIC-STRING}, CEE2F6. */
    BAD_PIC_STRING("000309D659C3C5C5", "Bad Pic String "),
    /** {@code FC-NON-NUMERIC-DATA}, CEE2F8. */
    NON_NUMERIC_DATA("000309D859C3C5C5", "Nonnumeric data"),
    /** {@code FC-YEAR-IN-ERA-ZERO}, CEE2F9. */
    YEAR_IN_ERA_ZERO("000309D959C3C5C5", "YearInEra is 0 ");

    /** {@code WS-RESULT} for any token not listed ({@code WHEN OTHER}). */
    public static final String OTHER_RESULT = "Date is invalid";

    private final byte[] token;
    private final String result;

    CeedaysFeedback(String tokenHex, String result) {
        this.token = HexFormat.of().parseHex(tokenHex);
        this.result = result;
    }

    /** The first 8 bytes of the feedback code ({@code FEEDBACK-TOKEN-VALUE}). */
    public byte[] token() {
        return token.clone();
    }

    /** {@code SEVERITY OF FEEDBACK-CODE}: bytes 0-1 of the token. */
    public int severity() {
        return (token[0] & 0xFF) << 8 | token[1] & 0xFF;
    }

    /** {@code MSG-NO OF FEEDBACK-CODE}: bytes 2-3 of the token. */
    public int messageNumber() {
        return (token[2] & 0xFF) << 8 | token[3] & 0xFF;
    }

    /** {@code WS-RESULT} as CSUTLDTC's EVALUATE moves it, 15 characters. */
    public String result() {
        return result;
    }

    /** The 88-level matching these 8 token bytes, or {@code null} for {@code WHEN OTHER}. */
    public static CeedaysFeedback ofToken(byte[] token) {
        for (CeedaysFeedback feedback : values()) {
            if (Arrays.equals(feedback.token, token)) {
                return feedback;
            }
        }
        return null;
    }
}
