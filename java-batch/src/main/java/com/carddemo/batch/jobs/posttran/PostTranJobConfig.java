package com.carddemo.batch.jobs.posttran;

import com.carddemo.batch.io.CardDemoData;
import com.carddemo.batch.io.FixedWidthItemIO;
import com.carddemo.batch.io.KsdsFile;
import com.carddemo.batch.model.AccountRecord;
import com.carddemo.batch.model.CardXrefRecord;
import com.carddemo.batch.model.DailyTransactionRecord;
import com.carddemo.batch.model.TranCatBalRecord;
import com.carddemo.batch.model.TransactionRecord;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.configuration.annotation.JobScope;
import org.springframework.batch.core.configuration.annotation.StepScope;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.item.file.FlatFileItemReader;
import org.springframework.batch.item.file.FlatFileItemWriter;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.io.FileSystemResource;
import org.springframework.transaction.PlatformTransactionManager;

/**
 * POSTTRAN (app/jcl/POSTTRAN.jcl, program CBTRN02C): posts the daily transaction file.
 *
 * <p>Job parameters: {@code inputDir} (DALYTRAN/XREFFILE/ACCTFILE/TCATBALF as the
 * {@code app/data/ASCII} file names; defaults to that directory) and {@code outputDir}, which
 * receives TRANSACT.txt, DALYREJS.txt, ACCTFILE.txt, TCATBALF.txt and SYSOUT.txt. The CBTRN02C
 * return code (4 when any record was rejected) is stored as {@code returnCode} in the job
 * execution context and as the exit description of the close step.
 */
@Configuration
public class PostTranJobConfig {

    public static final String JOB_NAME = "posttranJob";
    public static final String RETURN_CODE = "returnCode";
    static final int CHUNK_SIZE = 100;

    @Bean
    public Job posttranJob(JobRepository jobRepository, Step posttranPostStep, Step posttranCloseStep) {
        return new JobBuilder(JOB_NAME, jobRepository)
                .preventRestart()
                .start(posttranPostStep)
                .next(posttranCloseStep)
                .build();
    }

    @Bean
    public Step posttranPostStep(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                                 FlatFileItemReader<DailyTransactionRecord> posttranDalytranReader,
                                 PostingEngine posttranEngine,
                                 FlatFileItemWriter<RejectRecord> posttranDalyrejsWriter) {
        return new StepBuilder("posttranPostStep", jobRepository)
                .<DailyTransactionRecord, RejectRecord>chunk(CHUNK_SIZE, transactionManager)
                .reader(posttranDalytranReader)
                .processor(tran -> posttranEngine.process(tran).orElse(null))
                .writer(posttranDalyrejsWriter)
                .build();
    }

    @Bean
    public Step posttranCloseStep(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                                  PostingEngine posttranEngine, PostTranFiles posttranFiles) {
        return new StepBuilder("posttranCloseStep", jobRepository)
                .tasklet((contribution, chunkContext) -> {
                    posttranFiles.close(posttranEngine);
                    int rc = posttranEngine.returnCode();
                    chunkContext.getStepContext().getStepExecution().getJobExecution()
                            .getExecutionContext().putInt(RETURN_CODE, rc);
                    contribution.setExitStatus(ExitStatus.COMPLETED.addExitDescription("RC=" + rc));
                    return RepeatStatus.FINISHED;
                }, transactionManager)
                .build();
    }

    @Bean
    @JobScope
    public PostTranFiles posttranFiles(@Value("#{jobParameters['inputDir']}") String inputDir,
                                       @Value("#{jobParameters['outputDir']}") String outputDir) {
        Path in = inputDir == null ? CardDemoData.asciiDir() : Path.of(inputDir);
        if (outputDir == null) {
            throw new IllegalArgumentException("job parameter outputDir is required");
        }
        return new PostTranFiles(in, Path.of(outputDir));
    }

    @Bean
    @JobScope
    public PostingEngine posttranEngine(PostTranFiles posttranFiles, ObjectProvider<Clock> clock) {
        return posttranFiles.openEngine(clock.getIfAvailable(Clock::systemDefaultZone));
    }

    @Bean
    @StepScope
    public FlatFileItemReader<DailyTransactionRecord> posttranDalytranReader(PostTranFiles posttranFiles) {
        return FixedWidthItemIO.reader("posttranDalytranReader",
                new FileSystemResource(posttranFiles.input(CardDemoData.DAILY_TRANSACTIONS)),
                DailyTransactionRecord.CODEC);
    }

    @Bean
    @StepScope
    public FlatFileItemWriter<RejectRecord> posttranDalyrejsWriter(PostTranFiles posttranFiles) {
        return FixedWidthItemIO.writer("posttranDalyrejsWriter",
                new FileSystemResource(posttranFiles.output(PostTranFiles.DALYREJS)), RejectRecord.CODEC);
    }

    /** The job's datasets: indexed files held in memory and rewritten in key order at close. */
    public static class PostTranFiles {
        public static final String TRANSACT = "TRANSACT.txt";
        public static final String DALYREJS = "DALYREJS.txt";
        public static final String ACCTFILE = "ACCTFILE.txt";
        public static final String TCATBALF = "TCATBALF.txt";
        public static final String SYSOUT = "SYSOUT.txt";

        private final Path inputDir;
        private final Path outputDir;
        private KsdsFile<Long, AccountRecord> accounts;
        private KsdsFile<TranCatBalRecord.TranCatKey, TranCatBalRecord> tcatbal;
        private KsdsFile<String, TransactionRecord> transact;

        PostTranFiles(Path inputDir, Path outputDir) {
            this.inputDir = inputDir;
            this.outputDir = outputDir;
            try {
                Files.createDirectories(outputDir);
            } catch (IOException e) {
                throw new UncheckedIOException(e);
            }
        }

        public Path input(String name) {
            return inputDir.resolve(name);
        }

        public Path output(String name) {
            return outputDir.resolve(name);
        }

        public PostingEngine openEngine(Clock clock) {
            KsdsFile<String, CardXrefRecord> xref = KsdsFile.load(input(CardDemoData.CARD_XREF),
                    CardXrefRecord.CODEC, CardXrefRecord::xrefCardNum);
            accounts = KsdsFile.load(input(CardDemoData.ACCOUNTS), AccountRecord.CODEC, AccountRecord::acctId);
            tcatbal = KsdsFile.load(input(CardDemoData.TRAN_CAT_BALANCES), TranCatBalRecord.CODEC,
                    TranCatBalRecord::tranCatKey);
            transact = new KsdsFile<>(TransactionRecord.CODEC, TransactionRecord::tranId);
            return new PostingEngine(xref, accounts, tcatbal, transact, clock);
        }

        public void close(PostingEngine engine) throws IOException {
            transact.save(output(TRANSACT));
            accounts.save(output(ACCTFILE));
            tcatbal.save(output(TCATBALF));
            List<String> sysout = new ArrayList<>();
            sysout.add("START OF EXECUTION OF PROGRAM CBTRN02C");
            sysout.addAll(engine.sysout());
            sysout.add(String.format(Locale.ROOT, "TRANSACTIONS PROCESSED :%09d", engine.transactionCount()));
            sysout.add(String.format(Locale.ROOT, "TRANSACTIONS REJECTED  :%09d", engine.rejectCount()));
            sysout.add("END OF EXECUTION OF PROGRAM CBTRN02C");
            Files.write(output(SYSOUT), sysout, StandardCharsets.US_ASCII);
        }
    }
}
