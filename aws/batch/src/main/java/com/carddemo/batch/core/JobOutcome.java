package com.carddemo.batch.core;

import java.util.LinkedHashMap;
import java.util.Map;

/** Result of one job run: legacy return code plus counters (written to {@code runs/<runId>/<job>.json}). */
public record JobOutcome(int returnCode, Map<String, Object> counts, String message) {

    public static JobOutcome ok(Map<String, Object> counts) {
        return new JobOutcome(ReturnCode.OK, new LinkedHashMap<>(counts), null);
    }

    public static JobOutcome of(int returnCode, Map<String, Object> counts, String message) {
        return new JobOutcome(returnCode, new LinkedHashMap<>(counts), message);
    }
}
