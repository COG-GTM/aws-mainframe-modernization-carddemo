package com.carddemo.transaction.service;

/**
 * Outcome of {@code CBTRN02C 1500-VALIDATE-TRAN}: {@code WS-VALIDATION-FAIL-REASON} (0 = valid) and
 * {@code WS-VALIDATION-FAIL-REASON-DESC}, plus {@code XREF-ACCT-ID} that posting keys on (0 when the card was not found).
 */
public record ValidationResult(int reasonCode, String reasonDesc, long xrefAcctId) {

    public static final int INVALID_CARD = 100;
    public static final int ACCOUNT_NOT_FOUND = 101;
    public static final int OVERLIMIT = 102;
    public static final int AFTER_EXPIRATION = 103;

    public static ValidationResult valid(long xrefAcctId) {
        return new ValidationResult(0, "", xrefAcctId);
    }

    public boolean isValid() {
        return reasonCode == 0;
    }
}
