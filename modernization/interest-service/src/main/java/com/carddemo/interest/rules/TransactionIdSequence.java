package com.carddemo.interest.rules;

/**
 * Generates the 16-character transaction ids of an interest run.
 *
 * <p>Business rule BR-7a. COBOL {@code 1300-B-WRITE-TX}
 * ({@code app/cbl/CBACT04C.cbl:474-480}) increments {@code WS-TRANID-SUFFIX PIC 9(06)} and
 * concatenates the ten-character run date passed as the JCL {@code PARM}
 * ({@code app/jcl/INTCALC.jcl:22}, {@code PARM='2022071800'}) with that zero-padded suffix,
 * exactly filling {@code TRAN-ID PIC X(16)}. The counter is per run, not per account.
 */
public final class TransactionIdSequence {

    private static final int RUN_DATE_LENGTH = 10;
    private static final int SUFFIX_DIGITS = 6;

    private final String runDate;
    private int suffix;

    public TransactionIdSequence(String runDate) {
        if (runDate == null || runDate.length() != RUN_DATE_LENGTH) {
            throw new IllegalArgumentException(
                    "Run date parameter must be exactly " + RUN_DATE_LENGTH + " characters (JCL PARM)");
        }
        this.runDate = runDate;
    }

    /** Returns the next id, e.g. {@code 2022071800000001}. */
    public String next() {
        suffix++;
        return runDate + ("%0" + SUFFIX_DIGITS + "d").formatted(suffix);
    }
}
