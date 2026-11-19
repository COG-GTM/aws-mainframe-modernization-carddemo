package com.carddemo.transaction.batch;

import com.carddemo.common.batch.JclExitCodeMapper;
import com.carddemo.common.batch.ReturnCode;
import com.carddemo.common.batch.SysoutWriter;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.StepExecutionListener;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemStream;

/**
 * The observable frame of {@code CBTRN02C} around the record loop (FR-POSTTRAN-12/14, PR-CBTRN02C-01/16/17):
 * start line (cbl:194), {@code WS-TRANSACTION-COUNT} / {@code WS-REJECT-COUNT} (cbl:185-186), the two count lines,
 * {@code RETURN-CODE} 4 when a record was rejected (cbl:227-231) and the end line (cbl:233). The counters are saved in the
 * step ExecutionContext at every commit so a restart reports the totals of the whole input (D-012).
 * A failed step ends with {@code ABENDING PROGRAM} (cbl:708) when no specific error line already did.
 */
public class PosttranStepReport implements StepExecutionListener, ItemStream {

    static final String PROCESSED_KEY = "posttran.transaction-count";
    static final String REJECTED_KEY = "posttran.reject-count";

    private final SysoutWriter sysout;
    private long transactionCount;
    private long rejectCount;

    public PosttranStepReport(SysoutWriter sysout) {
        this.sysout = sysout;
    }

    @Override
    public void beforeStep(StepExecution stepExecution) {
        sysout.println("START OF EXECUTION OF PROGRAM CBTRN02C");
    }

    @Override
    public void open(ExecutionContext context) {
        transactionCount = context.getLong(PROCESSED_KEY, 0L);
        rejectCount = context.getLong(REJECTED_KEY, 0L);
    }

    @Override
    public void update(ExecutionContext context) {
        context.putLong(PROCESSED_KEY, transactionCount);
        context.putLong(REJECTED_KEY, rejectCount);
    }

    /** {@code ADD 1 TO WS-TRANSACTION-COUNT} (cbl:206), plus {@code ADD 1 TO WS-REJECT-COUNT} (cbl:214) for a reject. */
    void count(boolean rejected) {
        transactionCount++;
        if (rejected) {
            rejectCount++;
        }
    }

    @Override
    public ExitStatus afterStep(StepExecution stepExecution) {
        if (stepExecution.getStatus() != BatchStatus.COMPLETED) {
            sysout.abend();
            return null;
        }
        sysout.println("TRANSACTIONS PROCESSED :" + String.format("%09d", transactionCount));
        sysout.println("TRANSACTIONS REJECTED  :" + String.format("%09d", rejectCount));
        if (rejectCount > 0) {
            JclExitCodeMapper.raise(stepExecution.getJobExecution(), ReturnCode.WARNING);
        }
        sysout.println("END OF EXECUTION OF PROGRAM CBTRN02C");
        return null;
    }

    long transactionCount() {
        return transactionCount;
    }

    long rejectCount() {
        return rejectCount;
    }
}
