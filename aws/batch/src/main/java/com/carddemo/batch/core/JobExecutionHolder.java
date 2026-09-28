package com.carddemo.batch.core;

/** Hands the parsed parameters to the tasklet and the outcome back to {@link JobRunner} (one job per JVM). */
final class JobExecutionHolder {

    private static final ThreadLocal<JobParams> PARAMS = new ThreadLocal<>();
    private static final ThreadLocal<JobOutcome> OUTCOME = new ThreadLocal<>();
    private static final ThreadLocal<RuntimeException> ERROR = new ThreadLocal<>();

    private JobExecutionHolder() {
    }

    static void begin(JobParams params) {
        PARAMS.set(params);
        OUTCOME.remove();
        ERROR.remove();
    }

    static void execute(CardDemoJob job) {
        try {
            OUTCOME.set(job.run(PARAMS.get()));
        } catch (RuntimeException e) {
            ERROR.set(e);
        }
    }

    static JobOutcome outcome() {
        return OUTCOME.get();
    }

    static RuntimeException error() {
        return ERROR.get();
    }

    static void clear() {
        PARAMS.remove();
        OUTCOME.remove();
        ERROR.remove();
    }
}
