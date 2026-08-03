package com.carddemo.interestcalc.program;

/**
 * Implements CBACT04C 9999-ABEND-PROGRAM.
 *
 * <p>The COBOL paragraph issues {@code CALL 'CEE3ABD' USING ABCODE, TIMING} with abend code 999,
 * which terminates the job step. The Java equivalent aborts the batch step with the same intent:
 * the run must fail loudly rather than produce a partial TRANSACT file.
 */
public class AbendException extends RuntimeException {

    /** The {@code ABCODE} passed to {@code CEE3ABD}. */
    public static final int ABEND_CODE = 999;

    public AbendException(String message) {
        super("CBACT04C abended (code " + ABEND_CODE + "): " + message);
    }
}
