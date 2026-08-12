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
    private static final int SUFFIX_MODULUS = 1_000_000;

    private final String runDate;
    private int suffix;

    public TransactionIdSequence(String runDate) {
        if (runDate == null || runDate.length() != RUN_DATE_LENGTH) {
            throw new IllegalArgumentException(
                    "Run date parameter must be exactly " + RUN_DATE_LENGTH + " characters (JCL PARM)");
        }
        this.runDate = runDate;
    }

    /**
     * Returns the next id, e.g. {@code 2022071800000001}.
     *
     * <p>The suffix wraps back to {@code 000000} after {@code 999999}: {@code ADD 1 TO
     * WS-TRANID-SUFFIX} carries no {@code ON SIZE ERROR} ({@code app/cbl/CBACT04C.cbl:474}), so the
     * high-order digit is discarded and {@code TRAN-ID} stays exactly 16 characters. Ids repeat in
     * a run of more than a million transactions, on the mainframe as here.
     */
    public String next() {
        suffix = (suffix + 1) % SUFFIX_MODULUS;
        return runDate + ("%0" + SUFFIX_DIGITS + "d").formatted(suffix);
    }
}
