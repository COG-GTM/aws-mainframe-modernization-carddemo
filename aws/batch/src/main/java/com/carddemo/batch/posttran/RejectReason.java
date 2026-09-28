package com.carddemo.batch.posttran;

/** {@code CBTRN02C} {@code WS-VALIDATION-FAIL-REASON} / {@code -DESC}. */
public enum RejectReason {
    INVALID_CARD(100, "INVALID CARD NUMBER FOUND"),
    ACCOUNT_NOT_FOUND(101, "ACCOUNT RECORD NOT FOUND"),
    OVERLIMIT(102, "OVERLIMIT TRANSACTION"),
    EXPIRED_ACCOUNT(103, "TRANSACTION RECEIVED AFTER ACCT EXPIRATION"),
    ACCOUNT_REWRITE_FAILED(109, "ACCOUNT RECORD NOT FOUND");

    private final int code;
    private final String description;

    RejectReason(int code, String description) {
        this.code = code;
        this.description = description;
    }

    public int code() {
        return code;
    }

    public String description() {
        return description;
    }

    public static RejectReason of(int code) {
        for (RejectReason r : values()) {
            if (r.code == code) {
                return r;
            }
        }
        throw new IllegalArgumentException("Unknown reject reason " + code);
    }
}
