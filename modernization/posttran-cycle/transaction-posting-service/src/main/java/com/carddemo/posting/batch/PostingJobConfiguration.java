package com.carddemo.posting.batch;

import com.carddemo.posting.PostingProperties;
import com.carddemo.posting.domain.PostingOutcome;
import com.carddemo.posting.domain.TransactionPostingService;
import com.carddemo.recordio.layout.Transaction;
import com.carddemo.recordio.layout.TransactionLayout;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.StepExecutionListener;
import org.springframework.batch.core.configuration.annotation.StepScope;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.launch.support.ExitCodeMapper;
import org.springframework.batch.core.launch.support.SimpleJvmExitCodeMapper;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.item.ItemProcessor;
import org.springframework.batch.item.ItemReader;
import org.springframework.batch.item.ItemWriter;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.PlatformTransactionManager;

import java.time.Clock;
import java.util.Map;

/**
 * Job {@code dailyTransactionPostingJob} = POSTTRAN STEP15. One chunk-oriented step:
 * read DALYTRAN, validate+post, write TRANSACT / DALYREJS.
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
    @StepScope
    public PostingRunState postingRunState(PostingProperties properties) {
        return new PostingRunState(properties);
    }

    @Bean
    @StepScope
    public ItemReader<Transaction> dailyTransactionReader(PostingProperties properties, PostingRunState state) {
        return new FixedWidthItemReader<>(properties.dailyTransactions(), TransactionLayout.INSTANCE, state.encoding());
    }

    @Bean
    @StepScope
    public ItemProcessor<Transaction, PostingOutcome> postingProcessor(PostingRunState state, Clock postingClock) {
        TransactionPostingService service = new TransactionPostingService(state.ledger(), postingClock);
        return service::process;
    }

    @Bean
    @StepScope
    public ItemWriter<PostingOutcome> postingOutcomeWriter(PostingRunState state) {
        return chunk -> chunk.forEach(state::record);
    }

    @Bean
    @StepScope
    public StepExecutionListener postingRunListener(PostingRunState state) {
        return new StepExecutionListener() {
            @Override
            public ExitStatus afterStep(StepExecution stepExecution) {
                if (stepExecution.getStatus().isUnsuccessful()) {
                    return stepExecution.getExitStatus();
                }
                state.flush();
                LOG.info("TRANSACTIONS PROCESSED : {}", String.format("%09d", state.transactionCount()));
                LOG.info("TRANSACTIONS REJECTED  : {}", String.format("%09d", state.rejectCount()));
                stepExecution.getExecutionContext().putLong("transactionCount", state.transactionCount());
                stepExecution.getExecutionContext().putLong("rejectCount", state.rejectCount());
                return state.rejectCount() > 0 ? new ExitStatus(COMPLETED_WITH_REJECTS) : ExitStatus.COMPLETED;
            }
        };
    }

    @Bean
    public Step postDailyTransactionsStep(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                                          ItemReader<Transaction> dailyTransactionReader,
                                          ItemProcessor<Transaction, PostingOutcome> postingProcessor,
                                          ItemWriter<PostingOutcome> postingOutcomeWriter,
                                          StepExecutionListener postingRunListener) {
        return new StepBuilder("postDailyTransactions", jobRepository)
                .<Transaction, PostingOutcome>chunk(100, transactionManager)
                .reader(dailyTransactionReader)
                .processor(postingProcessor)
                .writer(postingOutcomeWriter)
                .listener(postingRunListener)
                .build();
    }

    @Bean
    public Job dailyTransactionPostingJob(JobRepository jobRepository, Step postDailyTransactionsStep) {
        return new JobBuilder(JOB_NAME, jobRepository)
                .start(postDailyTransactionsStep)
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
