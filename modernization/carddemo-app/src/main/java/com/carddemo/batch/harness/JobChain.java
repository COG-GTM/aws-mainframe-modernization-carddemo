package com.carddemo.batch.harness;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
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

    private record Pending(String stepName, String jobName,
                           Function<Map<String, JobOutcome>, JobParameters> parameters, JclCond cond) {
    }

    private final BatchJobLauncher launcher;
    private final List<Pending> steps = new ArrayList<>();

    public JobChain(BatchJobLauncher launcher) {
        this.launcher = launcher;
    }

    public JobChain step(String stepName, String jobName, JobParameters parameters, String cond) {
        return step(stepName, jobName, ran -> parameters, cond);
    }

    /**
     * A step whose parameters are resolved when it is reached, from the outcomes of the steps that ran before it
     * (keyed by step name), e.g. to read the generation an earlier step of the same stream wrote.
     */
    public JobChain step(String stepName, String jobName, Function<Map<String, JobOutcome>, JobParameters> parameters,
                         String cond) {
        Objects.requireNonNull(parameters, "parameters");
        steps.add(new Pending(stepName, jobName, parameters, cond == null ? JclCond.NONE : JclCond.parse(cond)));
        return this;
    }

    public Result run() {
        Map<String, ReturnCode> previous = new LinkedHashMap<>();
        Map<String, JobOutcome> ran = new LinkedHashMap<>();
        boolean abended = false;
        ReturnCode max = ReturnCode.OK;
        List<StepResult> results = new ArrayList<>();
        for (Pending pending : steps) {
            Step step = new Step(pending.stepName(), pending.jobName(),
                    pending.parameters().apply(Collections.unmodifiableMap(ran)), pending.cond());
            if (step.cond().bypass(previous, abended)) {
                results.add(new StepResult(step, null));
                continue;
            }
            JobOutcome outcome = launcher.run(step.jobName(), step.parameters());
            previous.put(step.stepName(), outcome.returnCode());
            ran.put(step.stepName(), outcome);
            abended |= outcome.abended();
            max = max.max(outcome.returnCode());
            results.add(new StepResult(step, outcome));
        }
        return new Result(List.copyOf(results), max, abended);
    }
}
