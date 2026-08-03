package com.carddemo.interestcalc.program;

import java.nio.file.Path;

/**
 * The inputs of one CBACT04C job step, i.e. the DD statements and the PARM of
 * {@code //STEP15 EXEC PGM=CBACT04C,PARM='2022071800'} in {@code app/jcl/INTCALC.jcl}.
 *
 * @param runDate the JCL PARM value, moved into {@code PARM-DATE PIC X(10)} and used as the
 *                first 10 characters of every generated {@code TRAN-ID}. Injected, never
 *                hardcoded, so the job can be re-run for any business date.
 */
public record InterestCalculationRequest(Path tranCatBalanceFile, Path cardXrefFile, Path accountFile,
                                         Path disclosureGroupFile, String runDate) {

    public InterestCalculationRequest {
        if (runDate == null || runDate.length() != 10) {
            throw new IllegalArgumentException("runDate must be exactly 10 characters (PARM-DATE PIC X(10))");
        }
    }
}
