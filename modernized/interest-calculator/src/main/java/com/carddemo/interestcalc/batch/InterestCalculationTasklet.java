package com.carddemo.interestcalc.batch;

import com.carddemo.interestcalc.file.DatasetFiles;
import com.carddemo.interestcalc.program.Cbact04cProgram;
import com.carddemo.interestcalc.program.InterestCalculationRequest;
import com.carddemo.interestcalc.program.InterestCalculationResult;
import java.nio.file.Path;
import java.time.Clock;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;

/**
 * The single step of the migrated job. CBACT04C is a stateful sequential scan with control
 * breaks on the account id, so it is migrated as a tasklet that runs the whole scan rather than
 * as a chunk-oriented step, which would break the control-break state across chunk boundaries.
 */
public class InterestCalculationTasklet implements Tasklet {

    /** The DD names of {@code app/jcl/INTCALC.jcl}, mapped onto files in the input directory. */
    public static final String TCATBALF = "tcatbal.txt";
    public static final String XREFFILE = "cardxref.txt";
    public static final String ACCTFILE = "acctdata.txt";
    public static final String DISCGRP = "discgrp.txt";
    public static final String TRANSACT_OUT = "transact.dat";
    public static final String ACCTFILE_OUT = "acctdata-after.dat";

    private static final Logger log = LoggerFactory.getLogger(InterestCalculationTasklet.class);

    private final Clock clock;
    private final String runDate;
    private final Path inputDirectory;
    private final Path outputDirectory;

    public InterestCalculationTasklet(Clock clock, String runDate, Path inputDirectory, Path outputDirectory) {
        this.clock = clock;
        this.runDate = runDate;
        this.inputDirectory = inputDirectory;
        this.outputDirectory = outputDirectory;
    }

    @Override
    public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext) {
        InterestCalculationResult result = new Cbact04cProgram(clock).run(new InterestCalculationRequest(
                inputDirectory.resolve(TCATBALF),
                inputDirectory.resolve(XREFFILE),
                inputDirectory.resolve(ACCTFILE),
                inputDirectory.resolve(DISCGRP),
                runDate));

        DatasetFiles.write(outputDirectory.resolve(TRANSACT_OUT), result.transactionRecordImages());
        DatasetFiles.write(outputDirectory.resolve(ACCTFILE_OUT), result.accountMasterAfter());

        contribution.incrementWriteCount(result.transactions().size());
        log.info("CBACT04C: read {} TCATBALF records, wrote {} TRANSACT records to {}",
                result.recordsRead(), result.transactions().size(), outputDirectory.resolve(TRANSACT_OUT));
        return RepeatStatus.FINISHED;
    }
}
