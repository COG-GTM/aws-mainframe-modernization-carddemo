package com.carddemo.posting.exception;

/**
 * Base type for failures that stop the posting run.
 *
 * <p>Replaces the COBOL pattern of inspecting a two-character {@code FILE STATUS} and calling
 * {@code 9999-ABEND-PROGRAM} ({@code app/cbl/CBTRN02C.cbl:707-711}). Note the distinction the
 * COBOL also makes: a transaction that fails <em>validation</em> is not a failure of the run, it
 * is a rejected record, and is modelled as data
 * ({@link com.carddemo.posting.domain.RejectedTransaction}) rather than as an exception.
 */
public abstract class PostingBatchException extends RuntimeException {

    private static final long serialVersionUID = 1L;

    protected PostingBatchException(String message) {
        super(message);
    }

    protected PostingBatchException(String message, Throwable cause) {
        super(message, cause);
    }
}
