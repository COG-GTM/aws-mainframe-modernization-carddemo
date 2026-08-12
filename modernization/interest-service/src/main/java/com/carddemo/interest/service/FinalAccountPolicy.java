package com.carddemo.interest.service;

/**
 * What to do with the last account of the run when the input stream ends.
 *
 * <p>CBACT04C only rewrites an account when the <em>next</em> record belongs to a different
 * account ({@code app/cbl/CBACT04C.cbl:194-199}). Its intended catch-up path — the {@code ELSE}
 * branch that performs {@code 1050-UPDATE-ACCOUNT} at end of file
 * ({@code app/cbl/CBACT04C.cbl:219-221}) — is unreachable, because {@code PERFORM UNTIL
 * END-OF-FILE = 'Y'} re-tests the condition as soon as the read sets the flag and leaves the loop.
 * The interest of the final account is therefore computed and its transactions are written, but
 * its account balance is never updated.
 *
 * <p>This enum makes that defect an explicit, auditable decision instead of an accident.
 */
public enum FinalAccountPolicy {

    /**
     * Reproduce the mainframe exactly, dropping the final account's balance update. Required for
     * bit-for-bit parity with the current production job.
     */
    MAINFRAME_PARITY,

    /**
     * Post the final account's interest as well — the behaviour the COBOL clearly intended. Use
     * once the business has signed off on the correction.
     */
    POST_FINAL_ACCOUNT
}
