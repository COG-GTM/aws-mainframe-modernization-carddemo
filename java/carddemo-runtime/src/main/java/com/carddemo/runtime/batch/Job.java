package com.carddemo.runtime.batch;

import java.util.ArrayList;
import java.util.List;

/**
 * A JCL job: steps run in order; after an ABEND the remaining steps are bypassed (no COND=EVEN/ONLY
 * support yet). Spring Batch jobs in carddemo-batch may wrap this or use it only for RC/abend semantics.
 */
public final class Job {

    private final String name;
    private final List<JobStep> steps;

    public Job(String name, List<JobStep> steps) {
        this.name = name;
        this.steps = List.copyOf(steps);
    }

    public String name() {
        return name;
    }

    public List<StepResult> run(DdBindings dd) {
        List<StepResult> results = new ArrayList<>();
        int highest = 0;
        boolean abended = false;
        for (JobStep step : steps) {
            if (abended || step.bypass(highest)) {
                results.add(new StepResult(step.stepName(), StepResult.Status.BYPASSED, 0));
                continue;
            }
            try {
                int rc = step.run(dd);
                highest = Math.max(highest, rc);
                results.add(new StepResult(step.stepName(), StepResult.Status.COMPLETED, rc));
            } catch (Abend a) {
                abended = true;
                results.add(new StepResult(step.stepName(), StepResult.Status.ABENDED, a.abendCode()));
            }
        }
        return results;
    }

    /** Highest return code of completed steps (MAXCC), or -1 if any step abended. */
    public static int maxcc(List<StepResult> results) {
        int max = 0;
        for (StepResult r : results) {
            if (r.status() == StepResult.Status.ABENDED) {
                return -1;
            }
            max = Math.max(max, r.code());
        }
        return max;
    }
}
