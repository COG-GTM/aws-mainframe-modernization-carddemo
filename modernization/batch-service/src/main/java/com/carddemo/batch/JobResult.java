package com.carddemo.batch;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Outcome of a batch step. returnCode follows the JCL convention (0 = OK, 4 = completed with warnings, e.g.
 * POSTTRAN rejects), so schedulers and Step Functions can branch the same way the legacy COND= logic did.
 */
public record JobResult(String job, int returnCode, Map<String, Object> counters, String output) {

    public static JobResult of(String job, int returnCode, String output, Object... kv) {
        Map<String, Object> counters = new LinkedHashMap<>();
        for (int i = 0; i < kv.length; i += 2) {
            counters.put((String) kv[i], kv[i + 1]);
        }
        return new JobResult(job, returnCode, counters, output);
    }
}
