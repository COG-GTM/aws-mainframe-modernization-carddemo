package com.carddemo.batch.core;

import java.util.Map;

/** A {@link JobFailure} that still reports the counters reached before the failure. */
public class JobFailureWithCounts extends JobFailure {

    private final Map<String, Object> counts;

    public JobFailureWithCounts(int returnCode, String message, Map<String, Object> counts, Throwable cause) {
        super(returnCode, message, cause);
        this.counts = Map.copyOf(counts);
    }

    public Map<String, Object> counts() {
        return counts;
    }
}
