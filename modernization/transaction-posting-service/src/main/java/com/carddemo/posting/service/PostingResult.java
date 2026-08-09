package com.carddemo.posting.service;

import com.carddemo.posting.domain.PostedTransaction;
import com.carddemo.posting.domain.RejectedTransaction;

import java.util.List;

/**
 * What one posting run did, in the order it did it.
 *
 * <p>Mirrors the run summary the COBOL displays and the return code it sets
 * ({@code app/cbl/CBTRN02C.cbl:226-232}).
 */
public record PostingResult(int transactionsRead,
                            List<PostedTransaction> posted,
                            List<RejectedTransaction> rejected) {

    public PostingResult {
        posted = List.copyOf(posted);
        rejected = List.copyOf(rejected);
    }

    /**
     * The job step return code: 4 when anything was rejected, 0 otherwise
     * ({@code app/cbl/CBTRN02C.cbl:229-231}). A rejected transaction is a warning, not a failure,
     * so the run itself still ends normally.
     */
    public int returnCode() {
        return rejected.isEmpty() ? 0 : 4;
    }
}
