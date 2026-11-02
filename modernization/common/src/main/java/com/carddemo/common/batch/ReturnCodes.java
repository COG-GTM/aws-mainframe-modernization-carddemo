package com.carddemo.common.batch;

import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.StepExecution;

import java.util.List;
import java.util.Objects;

/**
 * Step condition codes. A step that completes records its RC (COBOL {@code RETURN-CODE}) in its
 * execution context; a step that throws is an abend. The process exit code of a CLI launch is the
 * highest RC of the steps that ran, or {@link #ABEND} when a step failed.
 */
public final class ReturnCodes {

    public static final String KEY = "jcl.returnCode";
    public static final String BYPASSED = "jcl.bypassed";

    /** Exit code of a launch whose job failed (a step abended, or a COND-free failure). */
    public static final int ABEND = 16;
    /** Exit code of a launch rejected before any step ran (bad arguments, unknown job): a JCL error. */
    public static final int JCL_ERROR = 12;

    private ReturnCodes() {
    }

    /** {@code MOVE rc TO RETURN-CODE} for the running step. */
    public static void set(StepContribution contribution, int returnCode) {
        if (returnCode < 0 || returnCode > 4095) {
            throw new IllegalArgumentException("RC must be 0-4095: " + returnCode);
        }
        contribution.getStepExecution().getExecutionContext().putInt(KEY, returnCode);
        contribution.setExitStatus(contribution.getExitStatus().addExitDescription("RC=" + returnCode));
    }

    public static boolean bypassed(StepExecution step) {
        return step.getExecutionContext().containsKey(BYPASSED);
    }

    /** The RC of a step that ran and completed (0 when it did not set one); null when bypassed or not completed. */
    public static Integer of(StepExecution step) {
        if (bypassed(step) || step.getStatus() != BatchStatus.COMPLETED) {
            return null;
        }
        return step.getExecutionContext().getInt(KEY, 0);
    }

    /** RCs of the steps of the same job execution that ran before {@code current}. */
    public static List<Integer> prior(StepExecution current) {
        return current.getJobExecution().getStepExecutions().stream()
                .filter(step -> !Objects.equals(step.getId(), current.getId()))
                .map(ReturnCodes::of)
                .filter(Objects::nonNull)
                .toList();
    }

    /** Process exit code of a finished job execution. */
    public static int exitCode(JobExecution execution) {
        if (execution.getStatus() != BatchStatus.COMPLETED) {
            return ABEND;
        }
        return execution.getStepExecutions().stream()
                .map(ReturnCodes::of)
                .filter(Objects::nonNull)
                .mapToInt(Integer::intValue)
                .max()
                .orElse(0);
    }
}
