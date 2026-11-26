package com.carddemo.programs.cobswait;

import com.carddemo.programs.cobswait.copybook.MvswaitTime;
import com.carddemo.programs.cobswait.copybook.ParmValue;

/**
 * Java 21 port of app/cbl/COBSWAIT.cbl: batch utility that waits for the number of centiseconds
 * given on the first SYSIN record (JCL app/jcl/WAITSTEP.jcl, e.g. {@code 00003600} = 36 seconds).
 */
public final class Cobswait {

    /** {@code STOP RUN} return code (RETURN-CODE is never set, MVSWAIT returns R15=0). */
    public static final int RC_OK = 0;
    /** Exit code used by {@link #main} when the step ABENDs (e.g. S0C7). */
    public static final int RC_ABEND = 16;

    private final SysinReader sysin;
    private final MvsWait mvsWait;
    private final AlphanumericToNumericMove move;

    // WORKING-STORAGE SECTION
    private MvswaitTime mvswaitTime = MvswaitTime.ZERO;
    private ParmValue parmValue = ParmValue.SPACES;
    // Special register RETURN-CODE
    private int returnCode = RC_OK;

    public Cobswait(SysinReader sysin, MvsWait mvsWait, AlphanumericToNumericMove move) {
        this.sysin = sysin;
        this.mvsWait = mvsWait;
        this.move = move;
    }

    /** PROCEDURE DIVISION. Returns the step return code. */
    public int run() {
        initializeWorkingStorage();
        acceptParmValue();
        moveParmValueToMvswaitTime();
        callMvswait();
        return stopRun();
    }

    // Each STOP RUN ends the run unit, so every run starts with fresh WORKING-STORAGE.
    private void initializeWorkingStorage() {
        mvswaitTime = MvswaitTime.ZERO;
        parmValue = ParmValue.SPACES;
        returnCode = RC_OK;
    }

    // ACCEPT PARM-VALUE FROM SYSIN.
    void acceptParmValue() {
        // At end of SYSIN the receiving item is left unchanged.
        sysin.acceptRecord().ifPresent(rec -> parmValue = ParmValue.of(rec));
    }

    // MOVE PARM-VALUE TO MVSWAIT-TIME.
    void moveParmValueToMvswaitTime() {
        mvswaitTime = move.apply(parmValue);
    }

    // CALL 'MVSWAIT' USING MVSWAIT-TIME.
    void callMvswait() {
        // The CALL does not use ON EXCEPTION / RETURNING; R15 lands in RETURN-CODE.
        returnCode = mvsWait.call(mvswaitTime);
    }

    // STOP RUN.
    int stopRun() {
        return returnCode;
    }

    public ParmValue parmValue() {
        return parmValue;
    }

    public MvswaitTime mvswaitTime() {
        return mvswaitTime;
    }

    public static void main(String[] args) {
        AlphanumericToNumericMove move =
                AlphanumericToNumericMove.valueOf(
                        System.getProperty("cobswait.move", AlphanumericToNumericMove.IBM_ENTERPRISE.name()));
        Cobswait program = new Cobswait(LineSysinReader.of(System.in), new SleepingMvsWait(), move);
        int rc;
        try {
            rc = program.run();
        } catch (DataExceptionAbend e) {
            System.err.println("COBSWAIT ABEND " + e.getMessage());
            rc = RC_ABEND;
        }
        System.exit(rc);
    }
}
