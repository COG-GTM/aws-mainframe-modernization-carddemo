package com.carddemo.common.batch;

import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.JobExecution;

/**
 * Maps a finished job execution to its JCL condition code (D-015). A COMPLETED job returns the highest RC
 * its steps recorded with {@link #raise(JobExecution, int)} (0 when none did, like an unset {@code RETURN-CODE});
 * any other end state is an abend (exception, D-006) and returns {@link ReturnCode#ABEND}.
 */
public final class JclExitCodeMapper {

    /** Job {@code ExecutionContext} key holding the condition code set by the job's steps. */
    public static final String RETURN_CODE_KEY = "jcl.return-code";

    private JclExitCodeMapper() {
    }

    public static int map(JobExecution execution) {
        if (execution.getStatus() != BatchStatus.COMPLETED) {
            return ReturnCode.ABEND;
        }
        return execution.getExecutionContext().getInt(RETURN_CODE_KEY, ReturnCode.OK);
    }

    /** Records {@code rc} for the job, keeping the highest value set so far (JCL MAXCC semantics). */
    public static void raise(JobExecution execution, int rc) {
        if (rc < 0 || rc > ReturnCode.MAX) {
            throw new IllegalArgumentException("Condition code out of range 0.." + ReturnCode.MAX + ": " + rc);
        }
        int current = execution.getExecutionContext().getInt(RETURN_CODE_KEY, ReturnCode.OK);
        execution.getExecutionContext().putInt(RETURN_CODE_KEY, Math.max(current, rc));
    }
}
