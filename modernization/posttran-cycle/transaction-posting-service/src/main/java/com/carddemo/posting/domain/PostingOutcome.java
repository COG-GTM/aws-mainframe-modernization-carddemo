package com.carddemo.posting.domain;

import com.carddemo.recordio.layout.Transaction;

/** What happened to one daily transaction: it was posted to the master, or rejected with a reason. */
public sealed interface PostingOutcome {

    Transaction transaction();

    /** The transaction as written to TRANSACT, i.e. with the processing timestamp stamped. */
    record Posted(Transaction transaction) implements PostingOutcome {
    }

    /** The original daily record plus the reason; becomes one 430-byte DALYREJS record. */
    record Rejected(Transaction transaction, RejectReason reason) implements PostingOutcome {
    }
}
