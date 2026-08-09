package com.carddemo.posting.exception;

/**
 * A dataset behind one of the job's DD names could not be read or written.
 *
 * <p>Replaces the open/close/read abend paths of {@code CBTRN02C}, for example
 * {@code 0000-DALYTRAN-OPEN} ({@code app/cbl/CBTRN02C.cbl:236-252}).
 */
public final class DatasetAccessException extends PostingBatchException {

    private static final long serialVersionUID = 1L;

    public DatasetAccessException(String message, Throwable cause) {
        super(message, cause);
    }
}
