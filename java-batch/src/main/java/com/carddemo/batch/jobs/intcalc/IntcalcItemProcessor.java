package com.carddemo.batch.jobs.intcalc;

import com.carddemo.batch.io.KsdsFile;
import com.carddemo.batch.model.AccountRecord;
import com.carddemo.batch.model.TranCatBalRecord;
import com.carddemo.batch.model.TransactionRecord;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.StepExecutionListener;
import org.springframework.batch.item.ItemProcessor;

import java.nio.file.Path;

/**
 * Step-scoped processor: interest per TCATBALF record, filtered when the rate is zero. After the step it
 * rewrites the last account and saves the updated ACCTFILE in key order.
 */
public class IntcalcItemProcessor implements ItemProcessor<TranCatBalRecord, TransactionRecord>, StepExecutionListener {

    private static final Logger log = LoggerFactory.getLogger(IntcalcItemProcessor.class);

    private final InterestCalculator calculator;
    private final KsdsFile<Long, AccountRecord> accounts;
    private final Path accountOutput;

    public IntcalcItemProcessor(InterestCalculator calculator, KsdsFile<Long, AccountRecord> accounts,
                                Path accountOutput) {
        this.calculator = calculator;
        this.accounts = accounts;
        this.accountOutput = accountOutput;
    }

    @Override
    public TransactionRecord process(TranCatBalRecord item) {
        return calculator.process(item).orElse(null);
    }

    @Override
    public ExitStatus afterStep(StepExecution stepExecution) {
        if (!stepExecution.getStatus().isUnsuccessful()) {
            calculator.finish();
            accounts.save(accountOutput);
            log.info("CBACT04C: {} balance records, {} interest transactions, ACCTFILE written to {}",
                    calculator.recordCount(), calculator.transactionCount(), accountOutput);
        }
        return stepExecution.getExitStatus();
    }
}
