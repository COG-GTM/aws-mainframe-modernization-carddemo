package com.carddemo.batch.harness;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import org.springframework.batch.core.JobParameters;

/**
 * A JCL job stream: steps run in order, each a Spring Batch job, each guarded by its {@link JclCond} against the
 * RCs of the steps that ran before it. The stream's RC is the highest RC of the steps that ran (MAXCC); an abend
 * bypasses later steps unless they say {@code COND=EVEN}/{@code ONLY}.
 */
public final class JobChain {

    public record Step(String stepName, String jobName, JobParameters parameters, JclCond cond) {

        public Step {
            Objects.requireNonNull(stepName, "stepName");
            Objects.requireNonNull(jobName, "jobName");
            parameters = parameters == null ? new JobParameters() : parameters;
            cond = cond == null ? JclCond.NONE : cond;
        }
    }

    /** {@code outcome} is null when the step was bypassed by its {@code COND}. */
    public record StepResult(Step step, JobOutcome outcome) {

        public boolean bypassed() {
            return outcome == null;
        }
    }

    public record Result(List<StepResult> steps, ReturnCode maxReturnCode, boolean abended) {

        public StepResult step(String stepName) {
            return steps.stream().filter(s -> s.step().stepName().equals(stepName)).findFirst()
                    .orElseThrow(() -> new IllegalArgumentException("no step " + stepName));
        }
    }

    private final BatchJobLauncher launcher;
    private final List<Step> steps = new ArrayList<>();

    public JobChain(BatchJobLauncher launcher) {
        this.launcher = launcher;
    }

    public JobChain step(String stepName, String jobName, JobParameters parameters, String cond) {
        steps.add(new Step(stepName, jobName, parameters, cond == null ? JclCond.NONE : JclCond.parse(cond)));
        return this;
    }

    public Result run() {
        Map<String, ReturnCode> previous = new LinkedHashMap<>();
        boolean abended = false;
        ReturnCode max = ReturnCode.OK;
        List<StepResult> results = new ArrayList<>();
        for (Step step : steps) {
            if (step.cond().bypass(previous, abended)) {
                results.add(new StepResult(step, null));
                continue;
            }
            JobOutcome outcome = launcher.run(step.jobName(), step.parameters());
            previous.put(step.stepName(), outcome.returnCode());
            abended |= outcome.abended();
            max = max.max(outcome.returnCode());
            results.add(new StepResult(step, outcome));
        }
        return new Result(List.copyOf(results), max, abended);
    }
}
