package com.carddemo.report;

import org.springframework.boot.context.properties.ConfigurationProperties;

import java.nio.file.Path;

/**
 * TRANREPT.jcl datasets and parameters.
 *
 * @param transactionMaster   TRANSACT KSDS image (REPRO'd to TRANSACT.BKUP in STEP05R)
 * @param sortStartDate       SYMNAMES PARM-START-DATE of the SORT step (hard-coded '2022-01-01' in JCL)
 * @param sortEndDate         SYMNAMES PARM-END-DATE of the SORT step (hard-coded '2022-07-06' in JCL)
 * @param dateParm            DATEPARM 80-byte record read by CBTRN03C ("yyyy-mm-dd yyyy-mm-dd")
 * @param cardXref            CARDXREF KSDS
 * @param transactionTypes    TRANTYPE KSDS
 * @param transactionCategories TRANCATG KSDS
 * @param report              TRANREPT output, RECFM=FB LRECL=133
 * @param outOfRangePolicy    what to do when a record fails CBTRN03C's own date check; see {@link OutOfRangePolicy}
 */
@ConfigurationProperties(prefix = "carddemo.report")
public record ReportProperties(
        String encoding,
        Path transactionMaster,
        String sortStartDate,
        String sortEndDate,
        Path dateParm,
        Path cardXref,
        Path transactionTypes,
        Path transactionCategories,
        Path report,
        OutOfRangePolicy outOfRangePolicy) {

    public enum OutOfRangePolicy {
        /**
         * What the COBOL does: {@code NEXT SENTENCE} inside the in-line PERFORM (CBTRN03C lines 173-175)
         * jumps past {@code END-PERFORM.}, so the first out-of-range record ends the run without totals.
         */
        STOP_LIKE_COBOL,
        /** The probable intent: skip the record and continue. */
        SKIP_RECORD
    }
}
