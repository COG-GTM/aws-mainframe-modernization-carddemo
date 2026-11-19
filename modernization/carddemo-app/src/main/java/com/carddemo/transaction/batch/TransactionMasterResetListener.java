package com.carddemo.transaction.batch;

import com.carddemo.common.batch.JclSteps;
import com.carddemo.transaction.repository.TransactionRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.explore.JobExplorer;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemStream;
import org.springframework.transaction.support.TransactionTemplate;

/**
 * {@code OPEN OUTPUT TRANSACT-FILE} (cbl:256, quirk Q-01): a new POSTTRAN instance starts from an empty
 * {@code transaction.transactions}; a restart of a FAILED instance keeps what it committed (D-024, FR-POSTTRAN-11,
 * PR-CBTRN02C-14). Registered as the step's second stream so it runs after DALYTRAN opens and before DALYREJS opens, the
 * COBOL open order (cbl:195-200).
 */
public class TransactionMasterResetListener implements ItemStream {

    private static final Logger log = LoggerFactory.getLogger(TransactionMasterResetListener.class);

    private final StepExecution stepExecution;
    private final JobExplorer jobExplorer;
    private final TransactionRepository transactions;
    private final TransactionTemplate transactionTemplate;

    public TransactionMasterResetListener(StepExecution stepExecution, JobExplorer jobExplorer,
                                          TransactionRepository transactions, TransactionTemplate transactionTemplate) {
        this.stepExecution = stepExecution;
        this.jobExplorer = jobExplorer;
        this.transactions = transactions;
        this.transactionTemplate = transactionTemplate;
    }

    @Override
    public void open(ExecutionContext context) {
        if (JclSteps.isRestart(stepExecution.getJobExecution(), jobExplorer)) {
            log.info("POSTTRAN restart: transaction master kept (D-024)");
            return;
        }
        long removed = transactionTemplate.execute(status -> {
            long count = transactions.count();
            transactions.deleteAllInBatch();
            return count;
        });
        log.info("POSTTRAN new instance: transaction master emptied, {} rows removed (OPEN OUTPUT, D-024)", removed);
    }
}
