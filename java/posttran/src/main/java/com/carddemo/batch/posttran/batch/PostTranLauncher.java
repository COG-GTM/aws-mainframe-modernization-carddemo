package com.carddemo.batch.posttran.batch;

import com.carddemo.batch.posttran.PostTranProperties;
import com.carddemo.batch.posttran.cobol.Abend;
import com.carddemo.batch.posttran.cobol.RecordFormat;
import com.carddemo.batch.posttran.cobol.Sysout;
import com.carddemo.batch.posttran.cobol.ZonedDecimal;
import com.carddemo.batch.posttran.logic.PostingService;
import com.carddemo.batch.posttran.record.DailyTransaction;
import java.nio.file.Path;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.job.Job;
import org.springframework.batch.core.job.JobExecution;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.job.parameters.JobParametersBuilder;
import org.springframework.batch.core.launch.JobOperator;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.Step;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.stereotype.Component;
import org.springframework.transaction.PlatformTransactionManager;

/** The POSTTRAN job (JCL step STEP15 EXEC PGM=CBTRN02C) as a single chunk-oriented Spring Batch step. */
@Component
public class PostTranLauncher {

    public static final String JOB_NAME = "POSTTRAN";

    private final JobRepository jobRepository;
    private final JobOperator jobOperator;
    private final PlatformTransactionManager transactionManager;
    private final PostingService postingService;
    private final Sysout sysout;
    private final PostTranProperties properties;

    public PostTranLauncher(JobRepository jobRepository, JobOperator jobOperator,
            PlatformTransactionManager transactionManager, PostingService postingService, Sysout sysout,
            PostTranProperties properties) {
        this.jobRepository = jobRepository;
        this.jobOperator = jobOperator;
        this.transactionManager = transactionManager;
        this.postingService = postingService;
        this.sysout = sysout;
        this.properties = properties;
    }

    public PostTranResult run(Path dalytran, Path dalyrejs, RecordFormat format) {
        PostTranCounters counters = new PostTranCounters();
        DailyTransactionReader reader = new DailyTransactionReader(dalytran, format);
        RejectFileWriter rejects = new RejectFileWriter(dalyrejs, format);
        Step step = new StepBuilder("CBTRN02C", jobRepository)
                .<DailyTransaction, DailyTransaction>chunk(properties.chunkSize())
                .reader(reader)
                .writer(new PostingItemWriter(postingService, rejects, counters))
                .stream(rejects)
                .transactionManager(transactionManager)
                .build();
        Job job = new JobBuilder(JOB_NAME, jobRepository).start(step).build();

        sysout.display("START OF EXECUTION OF PROGRAM CBTRN02C");
        postingService.resetWorkingStorage();
        JobExecution execution;
        try {
            execution = jobOperator.start(job, new JobParametersBuilder()
                    .addLong("run.id", System.nanoTime())
                    .addString("dalytran", dalytran.toString())
                    .addString("dalyrejs", dalyrejs.toString())
                    .toJobParameters());
        } catch (Exception e) {
            return abend(e, counters);
        }
        if (execution.getStatus() != BatchStatus.COMPLETED) {
            Throwable cause = execution.getAllFailureExceptions().stream().findFirst()
                    .orElse(new IllegalStateException("POSTTRAN ended " + execution.getStatus()));
            return abend(cause, counters);
        }
        sysout.display("TRANSACTIONS PROCESSED :" + ZonedDecimal.encodeUnsigned(counters.transactionCount(), 9));
        sysout.display("TRANSACTIONS REJECTED  :" + ZonedDecimal.encodeUnsigned(counters.rejectCount(), 9));
        int rc = counters.rejectCount() > 0 ? PostTranResult.RC_REJECTS : PostTranResult.RC_OK;
        sysout.display("END OF EXECUTION OF PROGRAM CBTRN02C");
        return new PostTranResult(rc, counters.transactionCount(), counters.rejectCount(), null);
    }

    private PostTranResult abend(Throwable failure, PostTranCounters counters) {
        Abend abend = findAbend(failure);
        if (abend != null) {
            sysout.display(abend.getMessage());
            sysout.display(abend.ioStatusLine());
        } else {
            sysout.display("UNEXPECTED ERROR: " + failure);
        }
        sysout.display("ABENDING PROGRAM");
        return new PostTranResult(PostTranResult.RC_ABEND, counters.transactionCount(), counters.rejectCount(),
                abend != null ? abend : failure);
    }

    private static Abend findAbend(Throwable t) {
        for (Throwable c = t; c != null; c = c.getCause()) {
            if (c instanceof Abend a) {
                return a;
            }
        }
        return null;
    }
}
