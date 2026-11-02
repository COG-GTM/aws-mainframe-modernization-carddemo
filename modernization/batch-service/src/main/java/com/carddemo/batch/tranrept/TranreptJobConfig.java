package com.carddemo.batch.tranrept;

import com.carddemo.common.batch.BatchArguments;
import com.carddemo.common.batch.BatchJobParameters;
import com.carddemo.common.batch.DdFiles;
import com.carddemo.common.batch.JobRunDirectory;
import com.carddemo.common.batch.RecordSort;
import com.carddemo.common.batch.ReturnCodes;
import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.CodePage;
import com.carddemo.common.record.RecordFormat;
import com.carddemo.common.record.layout.CardXrefRecordLayout;
import com.carddemo.common.record.layout.TranCatRecordLayout;
import com.carddemo.common.record.layout.TranRecordLayout;
import com.carddemo.common.record.layout.TranTypeRecordLayout;
import java.nio.file.Path;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.carddemo.common.security.CallerTokenForwarder;
import com.carddemo.common.security.TokenService;
import org.springframework.context.ApplicationContext;
import org.springframework.http.MediaType;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.web.client.RestClient;

/**
 * TRANREPT ({@code app/jcl/TRANREPT.jcl}, and {@code app/proc/TRANREPT.prc} that CORPT00C submits):
 * <ol>
 *   <li>STEP05R REPROC: the TRANSACT KSDS unloaded to TRANSACT.BKUP(+1). Input is the
 *       {@code --dd=TRANSACT=} file when given (a POSTTRAN/COMBTRAN dump, byte for byte what REPROC
 *       reads), otherwise transaction-service's current transactions.</li>
 *   <li>STEP05R SORT: {@code SORT FIELDS=(TRAN-CARD-NUM,A)} with {@code INCLUDE} on TRAN-PROC-DT
 *       between the DATEPARM dates (CORPT00C writes the same dates into SYMNAMES and DATEPARM)
 *       → TRANSACT.DALY(+1).</li>
 *   <li>STEP10R CBTRN03C ({@link TransactionReportProgram}) → TRANREPT(+1). CARDXREF, TRANTYPE and
 *       TRANCATG are the {@code --dd=} files when given, otherwise card-service and
 *       transaction-service.</li>
 * </ol>
 * DATEPARM is {@code --dd=DATEPARM=<file>} (its first record) or {@code --parm='YYYY-MM-DD YYYY-MM-DD'}
 * (the record text); {@link #arguments(LocalDate, LocalDate)} builds the latter for an online submit.
 */
@Configuration
public class TranreptJobConfig {

    public static final String TRANREPT = "TRANREPT";
    public static final String JOB_NAME = "transactionReportJob";

    static final String TRANSACT = "TRANSACT";
    static final String TRANSACT_BKUP = "TRANSACT.BKUP";
    static final String TRANSACT_DALY = "TRANSACT.DALY";
    static final String DATEPARM = "DATEPARM";
    static final String CARDXREF = "CARDXREF";
    static final String TRANTYPE = "TRANTYPE";
    static final String TRANCATG = "TRANCATG";

    private static final int PROC_DATE_OFFSET = TranRecordLayout.TRAN_PROC_TS.offset();

    /** A TRANREPT launch for a date range, as CORPT00C submits TRANREPT.prc with the dates in DATEPARM. */
    public static BatchArguments arguments(LocalDate startDate, LocalDate endDate) {
        return BatchArguments.of(TRANREPT, null, DateParm.text(startDate, endDate));
    }

    /**
     * The HTTP submit forwards its caller's token to card-service and transaction-service; a
     * {@code --job=} launch (non-web context) signs as BATCH (architecture.md §3.1).
     */
    @Bean
    public CallerTokenForwarder callerTokenForwarder(ApplicationContext context, TokenService tokens) {
        return context instanceof WebApplicationContext
                ? CallerTokenForwarder.forCallers()
                : CallerTokenForwarder.forBatchProcess(tokens);
    }

    @Bean
    public RestClient cardRestClient(RestClient.Builder builder, CallerTokenForwarder forwarder,
                                     @Value("${carddemo.services.card-url:http://localhost:8083}") String baseUrl) {
        return builder.baseUrl(baseUrl).defaultHeader("Accept", MediaType.APPLICATION_JSON_VALUE)
                .requestInterceptor(forwarder).build();
    }

    @Bean
    public RestClient transactionRestClient(RestClient.Builder builder, CallerTokenForwarder forwarder,
                                            @Value("${carddemo.services.transaction-url:http://localhost:8084}") String baseUrl) {
        return builder.baseUrl(baseUrl).defaultHeader("Accept", MediaType.APPLICATION_JSON_VALUE)
                .requestInterceptor(forwarder).build();
    }

    @Bean
    public Job transactionReportJob(JobRepository jobRepository, Step unloadTransactionsStep,
                                    Step selectTransactionsStep, Step transactionReportStep) {
        return new JobBuilder(JOB_NAME, jobRepository)
                .start(unloadTransactionsStep)
                .next(selectTransactionsStep)
                .next(transactionReportStep)
                .build();
    }

    /** STEP05R REPROC: TRANSACT → TRANSACT.BKUP(+1). Also checks DATEPARM is given before anything runs. */
    @Bean
    public Step unloadTransactionsStep(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                                       @Qualifier("transactionRestClient") RestClient transactionRestClient) {
        return new StepBuilder("unloadTransactionsStep", jobRepository)
                .tasklet((contribution, chunkContext) -> {
                    JobParameters params = contribution.getStepExecution().getJobParameters();
                    CodePage codePage = BatchJobParameters.codePage(params);
                    dateParm(params, codePage);
                    Optional<Path> file = BatchJobParameters.dd(params, TRANSACT);
                    List<CobolRecord> records = file
                            .map(f -> DdFiles.read(f, TranRecordLayout.LAYOUT, codePage))
                            .orElseGet(() -> new ServiceTransactionSource(transactionRestClient).unload(codePage));
                    ExecutionContext context = contribution.getStepExecution().getExecutionContext();
                    context.putString(TRANSACT, file.map(Path::toString).orElse("transaction-service"));
                    context.putInt(TRANSACT_BKUP, DdFiles.write(BatchJobParameters.runDirectory(params), TRANSACT_BKUP,
                            records, BatchJobParameters.format(params)));
                    ReturnCodes.set(contribution, 0);
                    return RepeatStatus.FINISHED;
                }, transactionManager)
                .build();
    }

    /** STEP05R SORT: TRANSACT.BKUP(+1) → TRANSACT.DALY(+1), by card number, dates included. */
    @Bean
    public Step selectTransactionsStep(JobRepository jobRepository, PlatformTransactionManager transactionManager) {
        return new StepBuilder("selectTransactionsStep", jobRepository)
                .tasklet((contribution, chunkContext) -> {
                    JobParameters params = contribution.getStepExecution().getJobParameters();
                    CodePage codePage = BatchJobParameters.codePage(params);
                    JobRunDirectory generation = BatchJobParameters.runDirectory(params);
                    RecordFormat format = BatchJobParameters.format(params);
                    // Without a DATEPARM record the dates are spaces; CBTRN03C then reports nothing anyway.
                    DateParm window = dateParm(params, codePage).orElse(DateParm.of("", codePage));
                    List<CobolRecord> selected = RecordSort.fields(RecordSort.Key.ascending(TranRecordLayout.TRAN_CARD_NUM))
                            .include(record -> window.includes(record, PROC_DATE_OFFSET))
                            .sort(List.of(DdFiles.read(file(generation, TRANSACT_BKUP, format), TranRecordLayout.LAYOUT,
                                    codePage)), TranRecordLayout.LAYOUT);
                    contribution.getStepExecution().getExecutionContext()
                            .putInt(TRANSACT_DALY, DdFiles.write(generation, TRANSACT_DALY, selected, format));
                    ReturnCodes.set(contribution, 0);
                    return RepeatStatus.FINISHED;
                }, transactionManager)
                .build();
    }

    /** STEP10R CBTRN03C: TRANSACT.DALY(+1) → TRANREPT(+1). An abend fails the step and writes no report. */
    @Bean
    public Step transactionReportStep(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                                      @Qualifier("cardRestClient") RestClient cardRestClient,
                                      @Qualifier("transactionRestClient") RestClient transactionRestClient) {
        return new StepBuilder("transactionReportStep", jobRepository)
                .tasklet((contribution, chunkContext) -> {
                    JobParameters params = contribution.getStepExecution().getJobParameters();
                    CodePage codePage = BatchJobParameters.codePage(params);
                    JobRunDirectory generation = BatchJobParameters.runDirectory(params);
                    RecordFormat format = BatchJobParameters.format(params);
                    Optional<DateParm> dateParm = dateParm(params, codePage);
                    List<CobolRecord> tranfile = DdFiles.read(file(generation, TRANSACT_DALY, format),
                            TranRecordLayout.LAYOUT, codePage);
                    TransactionReportProgram.Result result = TransactionReportProgram.run(dateParm, tranfile,
                            referenceData(params, codePage, cardRestClient, transactionRestClient), codePage);
                    ExecutionContext context = contribution.getStepExecution().getExecutionContext();
                    dateParm.ifPresent(d -> context.putString("dateRange", d.startText() + " to " + d.endText()));
                    context.putInt("tranfileRead", result.read());
                    context.putInt("reported", result.reported());
                    context.putInt(TRANREPT, DdFiles.write(generation, TRANREPT,
                            TransactionReportProgram.records(result.lines(), codePage), format));
                    ReturnCodes.set(contribution, 0);
                    return RepeatStatus.FINISHED;
                }, transactionManager)
                .build();
    }

    /** The DATEPARM record: empty when the DD file has no record; an error when DATEPARM is not given. */
    static Optional<DateParm> dateParm(JobParameters params, CodePage codePage) {
        Optional<Path> file = BatchJobParameters.dd(params, DATEPARM);
        if (file.isPresent()) {
            return DdFiles.read(file.get(), DateParm.LAYOUT, codePage).stream().findFirst().map(DateParm::of);
        }
        String parm = params.getString(BatchJobParameters.PARM);
        if (parm != null) {
            return Optional.of(DateParm.of(parm, codePage));
        }
        throw new IllegalStateException("TRANREPT needs DATEPARM: --dd=DATEPARM=<file> or --parm='YYYY-MM-DD YYYY-MM-DD'");
    }

    private static ReferenceData referenceData(JobParameters params, CodePage codePage, RestClient cards,
                                               RestClient transactions) {
        ReferenceData services = new ServiceReferenceData(cards, transactions);
        ReferenceData xref = BatchJobParameters.dd(params, CARDXREF)
                .<ReferenceData>map(f -> new FileReferenceData(DdFiles.read(f, CardXrefRecordLayout.LAYOUT, codePage),
                        List.of(), List.of()))
                .orElse(services);
        ReferenceData types = BatchJobParameters.dd(params, TRANTYPE)
                .<ReferenceData>map(f -> new FileReferenceData(List.of(),
                        DdFiles.read(f, TranTypeRecordLayout.LAYOUT, codePage), List.of()))
                .orElse(services);
        ReferenceData categories = BatchJobParameters.dd(params, TRANCATG)
                .<ReferenceData>map(f -> new FileReferenceData(List.of(), List.of(),
                        DdFiles.read(f, TranCatRecordLayout.LAYOUT, codePage)))
                .orElse(services);
        return ReferenceData.combine(xref, types, categories);
    }

    /** The file DdFiles.write gave output DD {@code ddName} in this generation. */
    private static Path file(JobRunDirectory generation, String ddName, RecordFormat format) {
        return generation.dd(format.organization() == RecordFormat.Organization.LINE_SEQUENTIAL ? ddName + ".txt" : ddName);
    }
}
