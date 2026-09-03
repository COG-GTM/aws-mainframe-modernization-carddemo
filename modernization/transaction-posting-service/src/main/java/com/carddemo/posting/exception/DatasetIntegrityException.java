package com.carddemo.posting.exception;

/**
 * A dataset contradicted itself: a record that was read a moment ago can no longer be rewritten.
 *
 * <p>The COBOL equivalent is the {@code INVALID KEY} branch of the account rewrite, reason 109
 * ({@code app/cbl/CBTRN02C.cbl:555-558}) — a branch that can only be reached if the account
 * vanished between the read and the rewrite. See rule BR-13 of the logic map: the COBOL records
 * that reason and then ignores it, so it is raised here rather than silently dropped.
 */
public final class DatasetIntegrityException extends PostingBatchException {

    private static final long serialVersionUID = 1L;

    public DatasetIntegrityException(String message) {
        super(message);
    }
}
