package com.carddemo.transaction.service;

/**
 * {@code CBTRN02C 9999-ABEND-PROGRAM} ({@code CALL 'CEE3ABD'} U999, cbl:707-711): fails the step, never skipped (D-006);
 * the launch exits 16 (B-07, D-015). The SYSOUT error lines are written before it is thrown.
 */
public class PosttranAbendException extends RuntimeException {

    public PosttranAbendException(String message, Throwable cause) {
        super(message, cause);
    }
}
