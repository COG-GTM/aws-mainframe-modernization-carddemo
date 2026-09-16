package com.carddemo.posting.batch;

import com.carddemo.posting.PostingProperties;
import com.carddemo.posting.domain.DailyTransaction;
import com.carddemo.posting.domain.PostingOutcome;
import com.carddemo.posting.domain.TransactionPostingService;
import com.carddemo.recordio.layout.Transaction;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.configuration.annotation.JobScope;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.launch.support.ExitCodeMapper;
import org.springframework.batch.core.launch.support.SimpleJvmExitCodeMapper;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.batch.item.ItemProcessor;
import org.springframework.batch.item.ItemReader;
import org.springframework.batch.item.ItemWriter;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.PlatformTransactionManager;

import java.time.Clock;
import java.util.Map;

/**
 * Job {@code dailyTransactionPostingJob} = POSTTRAN STEP15, as two steps:
 * <ol>
 *   <li>{@code postDailyTransactions}: chunk-oriented read DALYTRAN, validate+post into the in-memory
 *       masters, collect TRANSACT / DALYREJS records;</li>
 *   <li>{@code closeDatasets}: the 9000-*-CLOSE paragraphs, persisting all four output datasets.</li>
 * </ol>
 * Persisting in its own tasklet step (not in a step listener) means a write failure fails the step
 * and the job, as a CLOSE failure abends CBTRN02C. Only when it succeeds is the reject count mapped
 * to {@code COMPLETED_WITH_REJECTS} / RETURN-CODE 4.
 */
@Configuration
public class PostingJobConfiguration {

    public static final String JOB_NAME = "dailyTransactionPostingJob";
    public static final String COMPLETED_WITH_REJECTS = "COMPLETED_WITH_REJECTS";
    private static final Logger LOG = LoggerFactory.getLogger(PostingJobConfiguration.class);

    @Bean
    public Clock postingClock() {
        return Clock.systemDefaultZone();
    }

    @Bean
    @JobScope
    public PostingRunState postingRunState(PostingProperties properties) {
        return new PostingRunState(properties);
    }

    @Bean
    @JobScope
    public ItemReader<DailyTransaction> dailyTransactionReader(PostingProperties properties, PostingRunState state) {
        return new FixedWidthItemReader<>(properties.dailyTransactions(), Transaction.LENGTH, state.encoding(),
                DailyTransaction::decode);
    }

    @Bean
    @JobScope
    public ItemProcessor<DailyTransaction, PostingOutcome> postingProcessor(PostingRunState state, Clock postingClock) {
        TransactionPostingService service = new TransactionPostingService(state.ledger(), postingClock);
        return service::process;
    }

    @Bean
    @JobScope
    public ItemWriter<PostingOutcome> postingOutcomeWriter(PostingRunState state) {
        return chunk -> chunk.forEach(state::record);
    }

    @Bean
    @JobScope
    public Tasklet closeDatasetsTasklet(PostingRunState state) {
        return (contribution, chunkContext) -> {
            state.flush();
            LOG.info("TRANSACTIONS PROCESSED : {}", String.format("%09d", state.transactionCount()));
            LOG.info("TRANSACTIONS REJECTED  : {}", String.format("%09d", state.rejectCount()));
            var jobContext = chunkContext.getStepContext().getStepExecution().getJobExecution().getExecutionContext();
            jobContext.putLong("transactionCount", state.transactionCount());
            jobContext.putLong("rejectCount", state.rejectCount());
            if (state.rejectCount() > 0) {
                contribution.setExitStatus(new ExitStatus(COMPLETED_WITH_REJECTS));
            }
            return RepeatStatus.FINISHED;
        };
    }

    @Bean
    public Step postDailyTransactionsStep(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                                          ItemReader<DailyTransaction> dailyTransactionReader,
                                          ItemProcessor<DailyTransaction, PostingOutcome> postingProcessor,
                                          ItemWriter<PostingOutcome> postingOutcomeWriter) {
        return new StepBuilder("postDailyTransactions", jobRepository)
                .<DailyTransaction, PostingOutcome>chunk(100, transactionManager)
                .reader(dailyTransactionReader)
                .processor(postingProcessor)
                .writer(postingOutcomeWriter)
                .build();
    }

    @Bean
    public Step closeDatasetsStep(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                                  Tasklet closeDatasetsTasklet) {
        return new StepBuilder("closeDatasets", jobRepository).tasklet(closeDatasetsTasklet, transactionManager).build();
    }

    @Bean
    public Job dailyTransactionPostingJob(JobRepository jobRepository, Step postDailyTransactionsStep, Step closeDatasetsStep) {
        return new JobBuilder(JOB_NAME, jobRepository)
                .start(postDailyTransactionsStep)
                .next(closeDatasetsStep)
                .build();
    }

    /** {@code IF WS-REJECT-COUNT > 0 MOVE 4 TO RETURN-CODE} (CBTRN02C lines 230-232). */
    @Bean
    public ExitCodeMapper postingExitCodeMapper() {
        SimpleJvmExitCodeMapper mapper = new SimpleJvmExitCodeMapper();
        mapper.setMapping(Map.of(COMPLETED_WITH_REJECTS, 4));
        return mapper;
    }
}
