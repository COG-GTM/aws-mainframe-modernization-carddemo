package com.carddemo.posting.domain;

/**
 * Why a daily transaction was not posted.
 *
 * <p>The code and description are the ones the COBOL moves into
 * {@code WS-VALIDATION-FAIL-REASON} / {@code WS-VALIDATION-FAIL-REASON-DESC}
 * ({@code app/cbl/CBTRN02C.cbl:180-182}); they are part of the {@code DALYREJS} record contract
 * and so are data, not log text.
 */
public enum RejectReason {

    /** 100 — no cross-reference record for the card ({@code app/cbl/CBTRN02C.cbl:385-387}). */
    INVALID_CARD_NUMBER(100, "INVALID CARD NUMBER FOUND"),

    /** 101 — the cross-referenced account does not exist ({@code app/cbl/CBTRN02C.cbl:397-399}). */
    ACCOUNT_NOT_FOUND(101, "ACCOUNT RECORD NOT FOUND"),

    /** 102 — the transaction would breach the credit limit ({@code app/cbl/CBTRN02C.cbl:410-412}). */
    OVER_LIMIT(102, "OVERLIMIT TRANSACTION"),

    /** 103 — the transaction post-dates account expiry ({@code app/cbl/CBTRN02C.cbl:417-419}). */
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
