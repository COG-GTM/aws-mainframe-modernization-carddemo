package com.carddemo.posting.domain;

/**
 * The validation failure codes of {@code CBTRN02C 1500-A-LOOKUP-XREF} / {@code 1500-B-LOOKUP-ACCT}
 * (app/cbl/CBTRN02C.cbl lines 380-421). Code and text are written verbatim into the 80-byte
 * reject trailer, so both are part of the output contract.
 */
public enum RejectReason {

    INVALID_CARD(100, "INVALID CARD NUMBER FOUND"),
    ACCOUNT_NOT_FOUND(101, "ACCOUNT RECORD NOT FOUND"),
    OVER_LIMIT(102, "OVERLIMIT TRANSACTION"),
    ACCOUNT_EXPIRED(103, "TRANSACTION RECEIVED AFTER ACCT EXPIRATION");

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
}
