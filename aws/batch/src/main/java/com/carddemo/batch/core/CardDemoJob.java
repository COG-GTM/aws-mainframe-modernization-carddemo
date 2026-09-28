package com.carddemo.batch.core;

/**
 * One legacy batch program. Implementations are Spring beans; {@link BatchJobConfig} wraps each in a Spring Batch
 * {@code Job} named {@link #name()} (batch.md §2 job name).
 */
public interface CardDemoJob {

    String name();

    /**
     * Runs the job. Returns the outcome (return code 0 or 4) or throws {@link JobFailure} for 8/12/16. Other
     * exceptions are mapped by {@link JobRunner}.
     */
    JobOutcome run(JobParams params);
}
