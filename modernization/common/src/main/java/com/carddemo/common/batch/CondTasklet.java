package com.carddemo.common.batch;

import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;

/**
 * Runs {@code delegate} only when the step's {@link JclCond} does not bypass it. A bypassed step
 * completes with exit code {@code NOOP}, sets no RC and lets the job go on, as JCL does; a step
 * that throws fails the job and no later step runs (an abend).
 */
public final class CondTasklet implements Tasklet {

    private final JclCond cond;
    private final Tasklet delegate;

    public CondTasklet(JclCond cond, Tasklet delegate) {
        this.cond = cond;
        this.delegate = delegate;
    }

    public static CondTasklet of(String cond, Tasklet delegate) {
        return new CondTasklet(JclCond.parse(cond), delegate);
    }

    @Override
    public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext) throws Exception {
        var step = contribution.getStepExecution();
        var prior = ReturnCodes.prior(step);
        if (cond.bypass(prior)) {
            step.getExecutionContext().putString(ReturnCodes.BYPASSED, cond + " with prior RCs " + prior);
            contribution.setExitStatus(ExitStatus.NOOP.addExitDescription("bypassed by " + cond));
            return RepeatStatus.FINISHED;
        }
        return delegate.execute(contribution, chunkContext);
    }
}
