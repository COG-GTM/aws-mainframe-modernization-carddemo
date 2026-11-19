package com.carddemo.transaction.batch;

import com.carddemo.common.batch.JclSteps;
import com.carddemo.common.batch.SysoutWriter;
import com.carddemo.common.cobol.CobolCharsets;
import com.carddemo.transaction.domain.DailyReject;
import com.carddemo.transaction.domain.DailyTransaction;
import com.carddemo.transaction.repository.TransactionRepository;
import com.carddemo.transaction.service.DailyTransactionPostingService;
import com.carddemo.transaction.service.DailyTransactionValidator;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.StepExecutionListener;
import org.springframework.batch.core.configuration.annotation.StepScope;
import org.springframework.batch.core.explore.JobExplorer;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.item.Chunk;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemProcessor;
import org.springframework.batch.item.ItemStream;
import org.springframework.batch.item.ItemStreamException;
import org.springframework.batch.item.ItemStreamReader;
import org.springframework.batch.item.ItemStreamWriter;
import org.springframework.batch.item.support.CompositeItemStream;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.support.TransactionTemplate;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.time.Clock;

/**
 * Job {@code POSTTRAN} ({@code app/jcl/POSTTRAN.jcl}), step {@code STEP15 EXEC PGM=CBTRN02C} (FR-POSTTRAN-01, NFR-01):
 * chunk-oriented, commit interval 1, restartable. Reader = DALYTRAN (B-01), processor = {@code 1500-VALIDATE-TRAN},
 * writer = posting (B-03, B-04, B-06) or DALYREJS (B-02). Streams open in the COBOL order (cbl:195-200): DALYTRAN,
 * TRANSACT (reset, D-024), DALYREJS; the record counters follow.
 */
@Configuration
public class PosttranJobConfig {

    public static final String JOB = "POSTTRAN";
    public static final String STEP = "STEP15";

    /** VSAM/QSAM FILE STATUS 35: OPEN of a file that does not exist. */
    static final String STATUS_FILE_NOT_FOUND = "35";
    /** FILE STATUS 04: record length does not match the fixed record length. */
    static final String STATUS_RECORD_LENGTH = "04";
    /** FILE STATUS 30: permanent I/O error. */
    static final String STATUS_IO_ERROR = "30";

    @Bean
    Job posttranJob(JobRepository jobRepository, Step posttranStep, JobExplorer jobExplorer,
                    @Value("${carddemo.batch.output-dir:output}") Path outputDir) {
        return JclSteps.job(JOB, jobRepository)
                .validator(new PosttranParametersValidator(jobExplorer, outputDir))
                .start(posttranStep)
                .build();
    }

    @Bean
    Step posttranStep(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                      ItemStreamReader<DailyTransaction> posttranReader, ItemStream transactionMasterReset,
                      ItemStreamWriter<DailyReject> posttranRejectWriter, PosttranStepReport posttranStepReport,
                      SysoutWriter posttranSysout, DailyTransactionValidator validator,
                      DailyTransactionPostingService posting, Clock clock) {
        CompositeItemStream streams = new CompositeItemStream();
        streams.register(posttranReader);
        streams.register(transactionMasterReset);
        streams.register(posttranRejectWriter);
        streams.register(posttranStepReport);
        ItemProcessor<DailyTransaction, PostingItem> validate = t -> new PostingItem(t, validator.validate(t));
        return JclSteps.<DailyTransaction, PostingItem>chunkStep(STEP, jobRepository, transactionManager)
                .reader(posttranReader::read)
                .processor(validate)
                .writer(new DailyTransactionPostingWriter(posting, posttranRejectWriter, posttranStepReport, posttranSysout, clock))
                .stream(streams)
                .listener((StepExecutionListener) posttranStepReport)
                .build();
    }

    @Bean
    @StepScope
    SysoutWriter posttranSysout(@Value("#{jobParameters['" + PosttranDatasets.SYSOUT + "']}") String sysout) {
        return new SysoutWriter(sysout == null ? null : Path.of(sysout), System.out);
    }

    @Bean
    @StepScope
    PosttranStepReport posttranStepReport(SysoutWriter posttranSysout) {
        return new PosttranStepReport(posttranSysout);
    }

    /**
     * DALYTRAN (cbl:29-32, 345-369). Open failure: {@code ERROR OPENING DALYTRAN} (cbl:247); read failure (e.g. a short
     * last record): {@code ERROR READING DALYTRAN FILE} (cbl:362); both abend. Encoding from
     * {@code carddemo.posttran.dalytran-encoding} (ISO-8859-1 or IBM037, D-038, D-045).
     */
    @Bean
    @StepScope
    ItemStreamReader<DailyTransaction> posttranReader(
            @Value("#{jobParameters['" + PosttranDatasets.DALYTRAN + "']}") String dalytran,
            @Value("${carddemo.posttran.dalytran-encoding:ISO-8859-1}") String encoding, SysoutWriter posttranSysout) {
        return new DailyTransactionReader(Path.of(dalytran), CobolCharsets.forName(encoding)) {
            @Override
            protected void doOpen() throws IOException {
                try {
                    super.doOpen();
                } catch (IOException e) {
                    posttranSysout.abend("ERROR OPENING DALYTRAN",
                            e instanceof NoSuchFileException ? STATUS_FILE_NOT_FOUND : STATUS_IO_ERROR);
                    throw e;
                }
            }

            @Override
            protected DailyTransaction doRead() throws IOException {
                try {
                    return super.doRead();
                } catch (ItemStreamException | IOException e) {
                    posttranSysout.abend("ERROR READING DALYTRAN FILE",
                            e instanceof ItemStreamException ? STATUS_RECORD_LENGTH : STATUS_IO_ERROR);
                    throw e;
                }
            }
        };
    }

    @Bean
    @StepScope
    ItemStream transactionMasterReset(@Value("#{stepExecution}") StepExecution stepExecution, JobExplorer jobExplorer,
                                      TransactionRepository transactions, PlatformTransactionManager transactionManager) {
        return new TransactionMasterResetListener(stepExecution, jobExplorer, transactions,
                new TransactionTemplate(transactionManager));
    }

    /**
     * DALYREJS (cbl:446-465). Open failure: {@code ERROR OPENING DALY REJECTS FILE} (cbl:302); write failure:
     * {@code ERROR WRITING TO REJECTS FILE} (cbl:460); both abend.
     */
    @Bean
    @StepScope
    ItemStreamWriter<DailyReject> posttranRejectWriter(@Value("#{stepExecution.jobExecution.jobParameters}") JobParameters parameters,
                                                       @Value("${carddemo.batch.output-dir:output}") Path outputDir,
                                                       SysoutWriter posttranSysout) {
        Path file = PosttranDatasets.dalyrejs(parameters, outputDir);
        return new DailyRejectWriter(file) {
            @Override
            public void open(ExecutionContext context) {
                try {
                    if (file.getParent() != null) {
                        Files.createDirectories(file.getParent());
                    }
                    super.open(context);
                } catch (IOException | ItemStreamException e) {
                    posttranSysout.abend("ERROR OPENING DALY REJECTS FILE", STATUS_IO_ERROR);
                    throw new ItemStreamException("Cannot open DALYREJS " + file, e);
                }
            }

            @Override
            public void write(Chunk<? extends DailyReject> chunk) throws IOException {
                try {
                    super.write(chunk);
                } catch (IOException e) {
                    posttranSysout.abend("ERROR WRITING TO REJECTS FILE", STATUS_IO_ERROR);
                    throw e;
                }
            }
        };
    }
}
