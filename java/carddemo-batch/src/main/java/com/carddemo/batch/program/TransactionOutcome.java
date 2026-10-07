package com.carddemo.batch.program;

/**
 * What CBTRN01C decided for one DALYTRAN record, in file order: the card number was looked up in
 * XREFFILE ({@code xrefFound}); if found, the cross-referenced account was looked up in ACCTFILE
 * ({@code acctId}, {@code acctFound}). {@code acctId} and {@code acctFound} are {@code null} when the
 * card was not found, because the COBOL program never reaches 3000-READ-ACCOUNT in that case.
 * Mirrors the row layout of {@code golden-files/CBTRN01C/outcomes.json}.
 */
public record TransactionOutcome(String tranId, String cardNum, boolean xrefFound, Long acctId, Boolean acctFound,
                                 Outcome outcome) {

    public enum Outcome { VERIFIED, CARD_NOT_FOUND, ACCOUNT_NOT_FOUND }

    public static TransactionOutcome cardNotFound(String tranId, String cardNum) {
        return new TransactionOutcome(tranId, cardNum, false, null, null, Outcome.CARD_NOT_FOUND);
    }

    public static TransactionOutcome accountLookedUp(String tranId, String cardNum, long acctId, boolean acctFound) {
        return new TransactionOutcome(tranId, cardNum, true, acctId, acctFound,
                acctFound ? Outcome.VERIFIED : Outcome.ACCOUNT_NOT_FOUND);
    }
}
