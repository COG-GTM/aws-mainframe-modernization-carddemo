package com.carddemo.programs.cbstm03a;

import java.util.List;

/**
 * Replaces the z/OS control-block walk (PSA -> TCB -> TIOT) CBSTM03A performs at
 * start-up: job name (TIOTNJOB), step name (TIOTJSTP) and the step's DD entries.
 */
public record JobContext(String jobName, String stepName, List<DdEntry> ddEntries) {

    /** One TIOT entry: TIOCDDNM and whether UCB-ADDR is non-null. */
    public record DdEntry(String ddName, boolean validUcb) {
    }

    public JobContext {
        ddEntries = List.copyOf(ddEntries);
    }
}
