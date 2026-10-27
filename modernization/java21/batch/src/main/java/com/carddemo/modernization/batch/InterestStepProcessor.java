package com.carddemo.modernization.batch;

import com.carddemo.modernization.interest.InterestAbendException;
import com.carddemo.modernization.interest.InterestCalculator;
import com.carddemo.modernization.interest.InterestInput;
import com.carddemo.modernization.interest.InterestRun;
import java.nio.file.Path;
import java.time.Clock;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.StepExecutionListener;
import org.springframework.batch.item.ItemProcessor;

/** CBACT04C step: runs {@link InterestCalculator} with the {@code parmDate} job parameter (INTCALC PARM). */
final class InterestStepProcessor implements ItemProcessor<InterestInput, InterestRun>, StepExecutionListener {
    private final CycleProperties properties;
    private final Clock clock;
    private InterestCalculator calculator;
    private Path sysout;

    InterestStepProcessor(CycleProperties properties, Clock clock) {
        this.properties = properties;
        this.clock = clock;
    }

    @Override
    public void beforeStep(StepExecution stepExecution) {
        calculator = new InterestCalculator(clock, stepExecution.getJobParameters().getString(RunDirectory.PARM_DATE));
        sysout = RunDirectory.of(properties.runRoot(), stepExecution.getJobParameters())
                .resolve(properties.interest().sysout());
    }

    @Override
    public InterestRun process(InterestInput input) {
        try {
            return calculator.run(input);
        } catch (InterestAbendException e) {
            FixedLengthFiles.writeNewText(sysout, e.displayLines());
            throw e;
        }
    }
}
