package com.carddemo.transaction.batch.posting;

import com.carddemo.transaction.CardDemoProperties;
import com.carddemo.transaction.batch.JclReturnCodes;
import com.carddemo.transaction.batch.ProgramBanner;
import com.carddemo.transaction.cobol.CobolFileException;
import com.carddemo.transaction.cobol.FileStatus;
import com.carddemo.transaction.cobol.Pic;
import com.carddemo.transaction.domain.DailyTransaction;
import com.carddemo.transaction.io.DailyTransactionFileReader;
import com.carddemo.transaction.io.RejectRecord;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.StepExecutionListener;
import org.springframework.batch.core.configuration.annotation.StepScope;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.launch.support.RunIdIncrementer;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.item.ItemProcessor;
import org.springframework.batch.item.file.FlatFileItemWriter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.io.FileSystemResource;
import org.springframework.transaction.PlatformTransactionManager;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * CBTRN02C (JCL POSTTRAN) as {@code TransactionPostingJob}: DALYTRAN -> validate -> post (TCATBALF,
 * ACCTFILE, TRANFILE) or reject (DALYREJS).
 */
@Configuration
public class TransactionPostingJobConfig {

    public static final String JOB_NAME = "TransactionPostingJob";
    private static final Logger log = LoggerFactory.getLogger(TransactionPostingJobConfig.class);

    @Bean(name = JOB_NAME)
    public Job transactionPostingJob(JobRepository jobRepository, Step transactionPostingStep) {
        return new JobBuilder(JOB_NAME, jobRepository)
                .incrementer(new RunIdIncrementer())
                .start(transactionPostingStep)
                .build();
    }

    @Bean
    public Step transactionPostingStep(JobRepository jobRepository, PlatformTransactionManager txManager,
                                       DailyTransactionFileReader postingDalytranReader,
                                       TransactionPostingService postingService,
                                       FlatFileItemWriter<RejectRecord> dalyrejsWriter,
                                       CardDemoProperties properties) {
        ItemProcessor<DailyTransaction, RejectRecord> processor = tran -> postingService.process(tran).orElse(null);
        return new StepBuilder("CBTRN02C", jobRepository)
                .<DailyTransaction, RejectRecord>chunk(properties.batch().chunkSize(), txManager)
                .reader(postingDalytranReader)
                .processor(processor)
                .writer(dalyrejsWriter)
                .listener(new ProgramBanner("CBTRN02C"))
                .listener(new PostingCounters())
                .build();
    }

    @Bean
    @StepScope
    public DailyTransactionFileReader postingDalytranReader(CardDemoProperties properties,
                                                       @Value("#{jobParameters['dalytran']}") String dalytran) {
        // job parameter 'dalytran' overrides the configured DD DALYTRAN dataset
        return new DailyTransactionFileReader(new FileSystemResource(dalytran != null ? dalytran : properties.batch().dalytranFile()));
    }

    /**
     * DD DALYREJS (GDG +1): one new reject dataset per job execution, RECLN 430. Created even when empty,
     * like the cataloged generation.
     */
    @Bean
    @StepScope
    public FlatFileItemWriter<RejectRecord> dalyrejsWriter(CardDemoProperties properties,
                                                           @Value("#{stepExecution.jobExecutionId}") Long executionId) {
        Path dir = Path.of(properties.batch().dalyrejsDir());
        try {
            Files.createDirectories(dir);
        } catch (IOException e) {
            throw new CobolFileException("DALYREJS", FileStatus.PERMANENT_IO_ERROR, "ERROR OPENING DALY REJECTS FILE", e);
        }
        Path file = dir.resolve("DALYREJS.G" + Pic.zeroFill(executionId, 4) + "V00.txt");
        FlatFileItemWriter<RejectRecord> writer = new FlatFileItemWriter<>();
        writer.setName("dalyrejsWriter");
        writer.setResource(new FileSystemResource(file));
        writer.setEncoding(StandardCharsets.US_ASCII.name());
        writer.setLineAggregator(RejectRecord::format);
        writer.setShouldDeleteIfEmpty(false);
        writer.setShouldDeleteIfExists(true);
        return writer;
    }

    /** Final DISPLAYs and {@code IF WS-REJECT-COUNT > 0 MOVE 4 TO RETURN-CODE}. */
    static class PostingCounters implements StepExecutionListener {
        @Override
        public ExitStatus afterStep(StepExecution step) {
            long processed = step.getReadCount();
            long rejected = step.getWriteCount();
            log.info("TRANSACTIONS PROCESSED :{}", Pic.zeroFill(processed, 9));
            log.info("TRANSACTIONS REJECTED  :{}", Pic.zeroFill(rejected, 9));
            step.getExecutionContext().putLong("TRANSACTIONS_PROCESSED", processed);
            step.getExecutionContext().putLong("TRANSACTIONS_REJECTED", rejected);
            step.getExecutionContext().putLong("TRANSACTIONS_POSTED", step.getFilterCount());
            if (ExitStatus.COMPLETED.getExitCode().equals(step.getExitStatus().getExitCode()) && rejected > 0) {
                return JclReturnCodes.completedWithRejects();
            }
            return null;
        }
    }
}
