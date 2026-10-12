package com.carddemo.zerotrust.audit;

/**
 * Outcome of a chain verification: whether the chain is intact, how many entries were
 * checked, the sequence number of the first bad entry (0 when valid) and the problem found.
 */
public record VerificationResult(boolean valid, long entriesChecked, long firstBadSeq, String problem) {

    public static VerificationResult valid(long entriesChecked) {
        return new VerificationResult(true, entriesChecked, 0, "");
    }

    public static VerificationResult broken(long entriesChecked, long firstBadSeq, String problem) {
        return new VerificationResult(false, entriesChecked, firstBadSeq, problem);
    }
}
