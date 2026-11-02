package com.carddemo.transaction.batch;

import com.carddemo.common.batch.BatchJobLauncher;
import com.carddemo.common.batch.BatchJobParameters;
import com.carddemo.common.batch.BatchJobRegistry;
import com.carddemo.common.batch.DdFiles;
import com.carddemo.common.batch.JobRunDirectory;
import com.carddemo.common.batch.RecordSort;
import com.carddemo.common.batch.ReturnCodes;
import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.CodePage;
import com.carddemo.common.record.layout.DalytranRecordLayout;
import com.carddemo.common.record.layout.TranRecordLayout;
import com.carddemo.transaction.repository.CategoryBalanceRepository;
import com.carddemo.transaction.repository.DailyTransactionRepository;
import com.carddemo.transaction.repository.TransactionRejectRepository;
import com.carddemo.transaction.repository.TransactionRepository;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.domain.Sort;
import org.springframework.transaction.PlatformTransactionManager;

/**
 * The transaction-service jobs, keyed by their JCL member: POSTTRAN (CBTRN02C), INTCALC (CBACT04C)
 * and COMBTRAN (DFSORT). Each job reads its input DDs from files when given ({@code --dd=}) and
 * writes its output DDs as fixed-width files into the run's generation directory
 * ({@code <output-dir>/<JOB>/<timestamp>/<DD>}); the business logic stays in {@link PostingService}
 * and {@link InterestCalculationService}.
 */
@Configuration
public class BatchConfig {

    public static final String POST_TRANSACTIONS_JOB = "postTransactionsJob";
    public static final String INTEREST_CALCULATION_JOB = "interestCalculationJob";
    public static final String COMBINE_TRANSACTIONS_JOB = "combineTransactionsJob";

    public static final String POSTTRAN = "POSTTRAN";
    public static final String INTCALC = "INTCALC";
    public static final String COMBTRAN = "COMBTRAN";

    private static final String STARTED_AT = "batch.startedAt";

    @Bean
    public BatchJobRegistry batchJobRegistry(@Qualifier(POST_TRANSACTIONS_JOB) Job postTransactionsJob,
                                             @Qualifier(INTEREST_CALCULATION_JOB) Job interestCalculationJob,
                                             @Qualifier(COMBINE_TRANSACTIONS_JOB) Job combineTransactionsJob) {
        Map<String, Job> jobs = new LinkedHashMap<>();
        jobs.put(POSTTRAN, postTransactionsJob);
        jobs.put(INTCALC, interestCalculationJob);
        jobs.put(COMBTRAN, combineTransactionsJob);
        return new BatchJobRegistry(jobs);
    }

    @Bean
    public BatchJobLauncher batchJobLauncher(JobLauncher jobLauncher, BatchJobRegistry registry,
                                             @Value("${carddemo.batch.output-dir:out}") Path outputDir,
                                             Clock clock) {
        return new BatchJobLauncher(jobLauncher, registry, outputDir, clock);
    }

    // ---- POSTTRAN: EXEC PGM=CBTRN02C, DALYTRAN in; TRANFILE, DALYREJS, TCATBALF out --------------

    @Bean
    public Job postTransactionsJob(JobRepository jobRepository, Step stageDailyTransactionsStep,
                                   Step postTransactionsStep, Step writePostingOutputsStep) {
        return new JobBuilder(POST_TRANSACTIONS_JOB, jobRepository)
                .start(stageDailyTransactionsStep)
                .next(postTransactionsStep)
                .next(writePostingOutputsStep)
                .build();
    }

    /**
     * DD DALYTRAN as a file: its records are staged into {@code daily_transactions}, the table
     * {@link PostingService} reads. A transaction id already staged (posted or not) is skipped, so
     * resubmitting the same file does not post it twice. Without the DD the staged rows are used.
     */
    @Bean
    public Step stageDailyTransactionsStep(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                                           DailyTransactionRepository dailyTransactions) {
        return new StepBuilder("stageDailyTransactionsStep", jobRepository)
                .tasklet((contribution, chunkContext) -> {
                    JobParameters params = contribution.getStepExecution().getJobParameters();
                    ExecutionContext context = contribution.getStepExecution().getExecutionContext();
                    var dalytran = BatchJobParameters.dd(params, "DALYTRAN");
                    if (dalytran.isEmpty()) {
                        context.putString("dalytran", "daily_transactions table");
                        return RepeatStatus.FINISHED;
                    }
                    List<CobolRecord> records = DdFiles.read(dalytran.get(), DalytranRecordLayout.LAYOUT,
                            BatchJobParameters.codePage(params));
                    int staged = 0;
                    int alreadyStaged = 0;
                    for (CobolRecord record : records) {
                        if (dailyTransactions.existsById(record.getTrimmed(DalytranRecordLayout.DALYTRAN_ID))) {
                            alreadyStaged++;
                        } else {
                            dailyTransactions.save(BatchRecords.toDailyTransaction(record));
                            staged++;
                        }
                    }
                    context.putString("dalytran", dalytran.get().toString());
                    context.putInt("dalytranRead", records.size());
                    context.putInt("staged", staged);
                    context.putInt("alreadyStaged", alreadyStaged);
                    return RepeatStatus.FINISHED;
                }, transactionManager)
                .build();
    }

    /**
     * CBTRN02C. {@code RETURN-CODE} is 4 when any transaction was rejected, as the program sets it.
     * With DD DALYTRAN bound only that file's unposted records are posted, in file order; without it
     * every pending row of {@code daily_transactions}.
     */
    @Bean
    public Step postTransactionsStep(JobRepository jobRepository,
                                     PlatformTransactionManager transactionManager,
                                     PostingService postingService,
                                     DailyTransactionRepository dailyTransactions) {
        return new StepBuilder("postTransactionsStep", jobRepository)
                .tasklet((contribution, chunkContext) -> {
                    jobContext(contribution).putString(STARTED_AT, Instant.now().toString());
                    JobParameters params = contribution.getStepExecution().getJobParameters();
                    PostingService.PostingReport report;
                    if (BatchJobParameters.dd(params, "DALYTRAN").isEmpty()) {
                        report = postingService.postPendingTransactions();
                    } else {
                        int posted = 0;
                        int rejected = 0;
                        Set<String> seen = new HashSet<>();
                        for (CobolRecord record : dalytranRecords(params)) {
                            String id = record.getTrimmed(DalytranRecordLayout.DALYTRAN_ID);
                            var daily = dailyTransactions.findById(id).filter(d -> !d.isProcessed());
                            if (!seen.add(id) || daily.isEmpty()) {
                                continue;
                            }
                            if (postingService.postOne(daily.get())) {
                                posted++;
                            } else {
                                rejected++;
                            }
                        }
                        report = new PostingService.PostingReport(posted, rejected);
                    }
                    ExecutionContext context = contribution.getStepExecution().getExecutionContext();
                    context.putInt("posted", report.posted());
                    context.putInt("rejected", report.rejected());
                    ReturnCodes.set(contribution, report.rejected() > 0 ? 4 : 0);
                    return RepeatStatus.FINISHED;
                }, transactionManager)
                .build();
    }

    /**
     * The output DDs: DALYREJS (this run's rejects, 430-byte records), TRANSACT (the TRANFILE master
     * after the run, by TRAN-ID) and TCATBALF (the category-balance master after the run, by key).
     * ACCTFILE lives in account-service and is not dumped here.
     */
    @Bean
    public Step writePostingOutputsStep(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                                        TransactionRejectRepository rejects, TransactionRepository transactions,
                                        CategoryBalanceRepository categoryBalances) {
        return new StepBuilder("writePostingOutputsStep", jobRepository)
                .tasklet((contribution, chunkContext) -> {
                    JobParameters params = contribution.getStepExecution().getJobParameters();
                    JobRunDirectory generation = BatchJobParameters.runDirectory(params);
                    CodePage codePage = BatchJobParameters.codePage(params);
                    var format = BatchJobParameters.format(params);
                    Instant startedAt = Instant.parse(jobContext(contribution).getString(STARTED_AT));
                    Map<String, CobolRecord> originals = new HashMap<>();
                    dalytranRecords(params).forEach(
                            r -> originals.putIfAbsent(r.getTrimmed(DalytranRecordLayout.DALYTRAN_ID), r));
                    ExecutionContext context = contribution.getStepExecution().getExecutionContext();
                    context.putInt("DALYREJS", DdFiles.write(generation, "DALYREJS",
                            rejects.findByRejectedAtGreaterThanEqualOrderByRejectIdAsc(startedAt).stream()
                                    .map(r -> BatchRecords.toRejectRecord(r, codePage, originals.get(r.getTranId()))).toList(), format));
                    context.putInt("TRANSACT", DdFiles.write(generation, "TRANSACT",
                            transactions.findAll(Sort.by("tranId")).stream()
                                    .map(t -> BatchRecords.toTranRecord(t, codePage)).toList(), format));
                    context.putInt("TCATBALF", DdFiles.write(generation, "TCATBALF",
                            categoryBalances.findAllByOrderByIdAcctIdAscIdTypeCdAscIdCatCdAsc().stream()
                                    .map(b -> BatchRecords.toCategoryBalanceRecord(b, codePage)).toList(), format));
                    return RepeatStatus.FINISHED;
                }, transactionManager)
                .build();
    }

    // ---- INTCALC: EXEC PGM=CBACT04C,PARM='2022071800'; SYSTRAN(+1) out ------------------------

    @Bean
    public Job interestCalculationJob(JobRepository jobRepository, Step interestCalculationStep,
                                      Step writeSystemTransactionsStep) {
        return new JobBuilder(INTEREST_CALCULATION_JOB, jobRepository)
                .start(interestCalculationStep)
                .next(writeSystemTransactionsStep)
                .build();
    }

    /**
     * CBACT04C. The PARM (default {@code yyyyMMdd00} of {@code --date}) is recorded for the run;
     * the service does not read it yet (the TRAN-ID it builds from PARM-DATE is UNT149-13's parity work).
     */
    @Bean
    public Step interestCalculationStep(JobRepository jobRepository,
                                        PlatformTransactionManager transactionManager,
                                        InterestCalculationService interestCalculationService,
                                        Clock clock) {
        return new StepBuilder("interestCalculationStep", jobRepository)
                .tasklet((contribution, chunkContext) -> {
                    JobParameters params = contribution.getStepExecution().getJobParameters();
                    jobContext(contribution).putString(STARTED_AT, LocalDateTime.now(clock).toString());
                    ExecutionContext context = contribution.getStepExecution().getExecutionContext();
                    context.putString("parm", BatchJobParameters.parm(params, clock));
                    InterestCalculationService.InterestReport report =
                            interestCalculationService.calculateInterest();
                    context.putInt("accountsSettled", report.accountsSettled());
                    context.putString("totalInterest", report.totalInterest().toPlainString());
                    ReturnCodes.set(contribution, 0);
                    return RepeatStatus.FINISHED;
                }, transactionManager)
                .build();
    }

    /** SYSTRAN(+1): the interest transactions this run wrote, by TRAN-ID. */
    @Bean
    public Step writeSystemTransactionsStep(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                                            TransactionRepository transactions) {
        return new StepBuilder("writeSystemTransactionsStep", jobRepository)
                .tasklet((contribution, chunkContext) -> {
                    JobParameters params = contribution.getStepExecution().getJobParameters();
                    CodePage codePage = BatchJobParameters.codePage(params);
                    LocalDateTime startedAt = LocalDateTime.parse(jobContext(contribution).getString(STARTED_AT));
                    contribution.getStepExecution().getExecutionContext().putInt("SYSTRAN", DdFiles.write(
                            BatchJobParameters.runDirectory(params), "SYSTRAN",
                            transactions.findBySourceAndProcTsGreaterThanEqualOrderByTranIdAsc(BatchRecords.SYSTEM_SOURCE, startedAt)
                                    .stream().map(t -> BatchRecords.toTranRecord(t, codePage)).toList(),
                            BatchJobParameters.format(params)));
                    return RepeatStatus.FINISHED;
                }, transactionManager)
                .build();
    }

    // ---- COMBTRAN: STEP05R SORT TRANSACT.BKUP(0)+SYSTRAN(0) -> TRANSACT.COMBINED(+1) -----------

    @Bean
    public Job combineTransactionsJob(JobRepository jobRepository, Step combineTransactionsSortStep) {
        return new JobBuilder(COMBINE_TRANSACTIONS_JOB, jobRepository).start(combineTransactionsSortStep).build();
    }

    /**
     * STEP05R: {@code SORT FIELDS=(TRAN-ID,A)} over TRANSACT.BKUP(0) and SYSTRAN(0). Each SORTIN is
     * the {@code --dd} file when given, else the DD of the latest TRANBKP / INTCALC generation under
     * the output directory. STEP10 (IDCAMS REPRO into the TRANSACT KSDS) has no Java step: INTCALC
     * already inserts its transactions into {@code transaction.transactions}.
     */
    @Bean
    public Step combineTransactionsSortStep(JobRepository jobRepository, PlatformTransactionManager transactionManager) {
        return new StepBuilder("combineTransactionsSortStep", jobRepository)
                .tasklet((contribution, chunkContext) -> {
                    JobParameters params = contribution.getStepExecution().getJobParameters();
                    JobRunDirectory generation = BatchJobParameters.runDirectory(params);
                    Path outputRoot = generation.path().getParent().getParent();
                    CodePage codePage = BatchJobParameters.codePage(params);
                    Path backup = sortin(params, outputRoot, "TRANSACT.BKUP", "TRANBKP");
                    Path systran = sortin(params, outputRoot, "SYSTRAN", INTCALC);
                    List<CobolRecord> combined = RecordSort.fields(RecordSort.Key.ascending(TranRecordLayout.TRAN_ID))
                            .sort(List.of(DdFiles.read(backup, TranRecordLayout.LAYOUT, codePage),
                                    DdFiles.read(systran, TranRecordLayout.LAYOUT, codePage)), TranRecordLayout.LAYOUT);
                    ExecutionContext context = contribution.getStepExecution().getExecutionContext();
                    context.putString("TRANSACT.BKUP", backup.toString());
                    context.putString("SYSTRAN", systran.toString());
                    context.putInt("TRANSACT.COMBINED", DdFiles.write(generation, "TRANSACT.COMBINED", combined,
                            BatchJobParameters.format(params)));
                    ReturnCodes.set(contribution, 0);
                    return RepeatStatus.FINISHED;
                }, transactionManager)
                .build();
    }

    private static Path sortin(JobParameters params, Path outputRoot, String dd, String producingJob) {
        return BatchJobParameters.dd(params, dd)
                .or(() -> JobRunDirectory.latestGeneration(outputRoot, producingJob, dd))
                .orElseThrow(() -> new IllegalStateException("SORTIN " + dd + "(0) not found: pass --dd=" + dd
                        + "=<file> or run " + producingJob + " first"));
    }

    private static List<CobolRecord> dalytranRecords(JobParameters params) {
        return BatchJobParameters.dd(params, "DALYTRAN")
                .map(file -> DdFiles.read(file, DalytranRecordLayout.LAYOUT, BatchJobParameters.codePage(params)))
                .orElse(List.of());
    }

    private static ExecutionContext jobContext(StepContribution contribution) {
        return contribution.getStepExecution().getJobExecution().getExecutionContext();
    }
}
