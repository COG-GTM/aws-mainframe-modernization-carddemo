package com.carddemo.transaction.batch.validation;

import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.annotation.BeforeStep;
import org.springframework.batch.item.Chunk;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemWriter;

/**
 * CBTRN01C writes nothing; this writer only tallies outcomes into the step ExecutionContext
 * (keys = {@link ValidationResult.Outcome} names).
 */
public class ValidationSummaryWriter implements ItemWriter<ValidationResult> {

    private ExecutionContext context;

    @BeforeStep
    public void beforeStep(StepExecution stepExecution) {
        this.context = stepExecution.getExecutionContext();
    }

    @Override
    public void write(Chunk<? extends ValidationResult> chunk) {
        for (ValidationResult r : chunk) {
            String key = r.outcome().name();
            context.putLong(key, context.getLong(key, 0L) + 1);
        }
    }
}
