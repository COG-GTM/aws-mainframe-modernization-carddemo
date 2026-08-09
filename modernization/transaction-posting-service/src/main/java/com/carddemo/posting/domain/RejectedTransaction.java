package com.carddemo.posting.domain;

/**
 * A daily transaction that failed validation, together with the reason.
 *
 * <p>Written to DD {@code DALYREJS} as the unchanged 350-byte transaction followed by an 80-byte
 * trailer ({@code app/cbl/CBTRN02C.cbl:446-448}).
 */
public record RejectedTransaction(DailyTransaction transaction, RejectReason reason) {
}
