package com.carddemo.common.error;

/**
 * The record was rewritten by someone else between our read and our rewrite: the case COACTUPC
 * and COCRDUPC report as "Record changed by some one else. Please review" after re-reading with
 * {@code READ ... UPDATE}. Raised from a JPA {@code @Version} conflict.
 */
public class RecordChangedException extends RuntimeException {

    public static final String MESSAGE = "Record changed by some one else. Please review";

    public RecordChangedException() {
        super(MESSAGE);
    }
}
