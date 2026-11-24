package com.carddemo.transaction.batch;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.StepExecutionListener;

/**
 * {@code DISPLAY 'START OF EXECUTION OF PROGRAM ...'} / {@code 'END OF EXECUTION OF PROGRAM ...'}.
 */
public class ProgramBanner implements StepExecutionListener {

    private static final Logger log = LoggerFactory.getLogger(ProgramBanner.class);

    private final String program;

    public ProgramBanner(String program) {
        this.program = program;
    }

    @Override
    public void beforeStep(StepExecution stepExecution) {
        log.info("START OF EXECUTION OF PROGRAM {}", program);
    }

    @Override
    public ExitStatus afterStep(StepExecution stepExecution) {
        if (!stepExecution.getFailureExceptions().isEmpty()) {
            log.error("ABENDING PROGRAM {}: {}", program, stepExecution.getFailureExceptions().get(0).getMessage());
            return null;
        }
        log.info("END OF EXECUTION OF PROGRAM {}", program);
        return null;
    }
}
