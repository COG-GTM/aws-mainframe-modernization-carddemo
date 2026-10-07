package com.carddemo.batch.posttran;

import com.carddemo.account.Account;
import com.carddemo.account.AccountRecord;
import com.carddemo.account.AccountRepository;
import com.carddemo.batch.BatchOutputFile;
import com.carddemo.batch.BatchOutputProperties;
import com.carddemo.batch.DatedOutputFiles;
import com.carddemo.batch.harness.BatchCommandLine;
import com.carddemo.batch.harness.BatchJobLauncher;
import com.carddemo.batch.harness.BufferedSink;
import com.carddemo.batch.harness.DdParameters;
import com.carddemo.batch.harness.FixedFileSink;
import com.carddemo.batch.harness.JobChain;
import com.carddemo.batch.harness.JobStream;
import com.carddemo.batch.harness.KeyedDataset;
import com.carddemo.batch.harness.KsdsInput;
import com.carddemo.batch.harness.RecordSink;
import com.carddemo.batch.harness.ReturnCode;
import com.carddemo.batch.harness.Sysout;
import com.carddemo.batch.print.ProgramCounts;
import com.carddemo.card.Card;
import com.carddemo.card.CardRecord;
import com.carddemo.card.CardRepository;
import com.carddemo.card.CardXref;
import com.carddemo.card.CardXrefRecord;
import com.carddemo.card.CardXrefRepository;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.data.CopybookRecordMapper;
import com.carddemo.customer.Customer;
import com.carddemo.customer.CustomerRecord;
import com.carddemo.customer.CustomerRepository;
import com.carddemo.transaction.DailyTransaction;
import com.carddemo.transaction.DailyTransactionRecord;
import com.carddemo.transaction.DailyTransactionRepository;
import com.carddemo.transaction.TranCatBalance;
import com.carddemo.transaction.TranCatBalanceId;
import com.carddemo.transaction.TranCatBalanceRecord;
import com.carddemo.transaction.TranCatBalanceRepository;
import com.carddemo.transaction.Transaction;
import com.carddemo.transaction.TransactionRecord;
import com.carddemo.transaction.TransactionRepository;
import java.nio.file.Path;
import java.time.Clock;
import java.util.List;
import java.util.Locale;
import java.util.function.BiFunction;
import java.util.function.Function;
import javax.sql.DataSource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.domain.Limit;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.TransactionDefinition;
import org.springframework.transaction.support.TransactionTemplate;

/**
 * POSTTRAN ({@code app/jcl/POSTTRAN.jcl}) as a harness job stream (ADR-0015): {@code STEP10} runs {@code cbtrn01c}
 * (CBTRN01C, the read-only DALYTRAN/XREF/account check), {@code STEP15} runs {@code cbtrn02c} (CBTRN02C, the posting
 * engine, the JCL's only step) with {@code COND=(4,LT,STEP10)}, so posting is bypassed when the check ends above RC 4
 * or abends. Every DD is a table unless {@code --<DD>=<path>} names an unload file (KSDS files are updated in place,
 * TRANFILE is recreated as CBTRN02C opens it OUTPUT). DALYREJS defaults to the dated generation
 * {@code AWS.M2.CARDDEMO.DALYREJS(+1)} (GDG base {@code DALYREJS}) registered in {@code batch_output_file} (ADR-0012); it is catalogued when
 * the step ends without an abend, as {@code DISP=(NEW,CATLG,DELETE)}. In table mode each daily transaction's reads
 * and updates commit as one database transaction.
 */
@Configuration(proxyBeanMethods = false)
public class PosttranJobConfiguration {

    private static final Logger log = LoggerFactory.getLogger(PosttranJobConfiguration.class);

    public static final String POSTTRAN = "posttran";
    public static final String CBTRN01C_JOB = "cbtrn01c";
    public static final String CBTRN02C_JOB = "cbtrn02c";
    public static final String STEP10 = "STEP10";
    public static final String STEP15 = "STEP15";
    public static final String STEP15_COND = "(4,LT,STEP10)";
    public static final String DALYREJS_GDG = "DALYREJS";
    public static final String SYSOUT_CONTEXT_KEY = "SYSOUT";
    public static final String DALYREJS_CONTEXT_KEY = "DALYREJS";

    static final int PAGE_SIZE = 500;

    @FunctionalInterface
    interface ProgramStep {
        ReturnCode run(StepExecution step, StepContribution contribution, JobParameters parameters, Sysout sysout,
                       RecordEncoding encoding);
    }

    @Bean
    JobStream posttranStream() {
        return new JobStream() {
            @Override
            public String name() {
                return POSTTRAN;
            }

            @Override
            public JobChain chain(BatchJobLauncher launcher, JobParameters parameters) {
                return new JobChain(launcher)
                        .step(STEP10, CBTRN01C_JOB, JobStream.forStep(parameters, STEP10), null)
                        .step(STEP15, CBTRN02C_JOB, JobStream.forStep(parameters, STEP15), STEP15_COND);
            }
        };
    }

    @Bean
    Job cbtrn01cJob(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                    BatchOutputProperties output, DailyTransactionRepository dailyTransactions,
                    CustomerRepository customers, CardXrefRepository xrefs, CardRepository cards,
                    AccountRepository accounts, TransactionRepository transactions) {
        return job(CBTRN01C_JOB, STEP10, jobRepository, transactionManager, output,
                (step, contribution, parameters, sysout, encoding) -> {
                    ProgramCounts counts = new Cbtrn01c(
                            dalytran(parameters, encoding, dailyTransactions),
                            input(parameters, Cbtrn01c.CUSTFILE, encoding, CustomerRecord.MAPPER, -1,
                                    customers::findByCustIdGreaterThanOrderByCustIdAsc, Customer::getCustId,
                                    Customer::toRecord),
                            xref(parameters, encoding, xrefs),
                            input(parameters, Cbtrn01c.CARDFILE, encoding, CardRecord.MAPPER, "",
                                    cards::findByCardNumGreaterThanOrderByCardNumAsc, Card::getCardNum,
                                    Card::toRecord),
                            account(parameters, encoding, accounts, KeyedDataset.Mode.INPUT),
                            input(parameters, Cbtrn01c.TRANFILE, encoding, TransactionRecord.MAPPER, "",
                                    transactions::findByTranIdGreaterThanOrderByTranIdAsc, Transaction::getTranId,
                                    Transaction::toRecord),
                            sysout).run();
                    for (long i = 0; i < counts.read(); i++) {
                        contribution.incrementReadCount();
                    }
                    return ReturnCode.OK;
                });
    }

    @Bean
    Job cbtrn02cJob(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                    BatchOutputProperties output, DatedOutputFiles datedOutputFiles, Clock clock,
                    DailyTransactionRepository dailyTransactions, CardXrefRepository xrefs,
                    AccountRepository accounts, TranCatBalanceRepository balances,
                    TransactionRepository transactions, DataSource dataSource) {
        TransactionTemplate perRecord = new TransactionTemplate(transactionManager);
        perRecord.setPropagationBehavior(TransactionDefinition.PROPAGATION_REQUIRES_NEW);
        Cbtrn02c.UnitOfWork unit = work -> perRecord.executeWithoutResult(status -> work.run());
        // Earlier records commit on their own, so re-running a failed instance would post them twice;
        // as on the mainframe, recovery is restore ACCTDATA/TCATBALF, then a new run.
        return job(CBTRN02C_JOB, STEP15, jobRepository, transactionManager, output, false,
                (step, contribution, parameters, sysout, encoding) -> {
                    boolean generation = DdParameters.isTable(parameters, Cbtrn02c.DALYREJS);
                    BufferedSink buffered = new BufferedSink(Cbtrn02c.DALYREJS);
                    RecordSink rejects = generation ? buffered
                            : new FixedFileSink(Cbtrn02c.DALYREJS, DdParameters.path(parameters, Cbtrn02c.DALYREJS));
                    Cbtrn02c.Result result;
                    try (TransactionIdStepLock idLock = DdParameters.isTable(parameters, Cbtrn02c.TRANFILE)
                            ? TransactionIdStepLock.acquire(dataSource) : null) {
                        result = new Cbtrn02c(
                                dalytran(parameters, encoding, dailyTransactions),
                                transactions(parameters, encoding, transactions, unit),
                                xref(parameters, encoding, xrefs),
                                rejects,
                                account(parameters, encoding, accounts, KeyedDataset.Mode.I_O),
                                balances(parameters, encoding, balances),
                                sysout, clock, unit).run();
                    }
                    String rejectsPath;
                    if (generation) {
                        BatchOutputFile file = datedOutputFiles.write(DALYREJS_GDG,
                                parameters.getLocalDate(BatchCommandLine.RUN_DATE), step.getJobExecutionId(),
                                buffered.records());
                        rejectsPath = file.getFilePath();
                    } else {
                        rejectsPath = DdParameters.path(parameters, Cbtrn02c.DALYREJS).toAbsolutePath().toString();
                    }
                    step.getExecutionContext().putString(DALYREJS_CONTEXT_KEY, rejectsPath);
                    log.info("{}: {} processed, {} posted, {} rejected -> DALYREJS {}", CBTRN02C_JOB,
                            result.processed(), result.posted(), result.rejected(), rejectsPath);
                    for (long i = 0; i < result.processed(); i++) {
                        contribution.incrementReadCount();
                    }
                    contribution.incrementWriteCount(result.posted());
                    contribution.incrementFilterCount(result.rejected());
                    return result.returnCode();
                });
    }

    static String pad(String value, int length) {
        return String.format(Locale.ROOT, "%-" + length + "s", value == null ? "" : value);
    }

    private static KsdsInput dalytran(JobParameters parameters, RecordEncoding encoding,
                                      DailyTransactionRepository dailyTransactions) {
        return input(parameters, Cbtrn02c.DALYTRAN, encoding, DailyTransactionRecord.MAPPER, -1,
                dailyTransactions::findByRecordSeqGreaterThanOrderByRecordSeqAsc, DailyTransaction::getRecordSeq,
                DailyTransaction::toRecord);
    }

    private static KeyedDataset<String, CardXrefRecord> xref(JobParameters parameters, RecordEncoding encoding,
                                                             CardXrefRepository xrefs) {
        String dd = Cbtrn02c.XREFFILE;
        if (!DdParameters.isTable(parameters, dd)) {
            return KeyedDataset.file(dd, DdParameters.path(parameters, dd), KeyedDataset.Mode.INPUT,
                    CardXrefRecord.MAPPER, CardXrefRecord::cardNum, k -> pad(k, 16), encoding);
        }
        return KeyedDataset.table(dd, KeyedDataset.Mode.INPUT, k -> xrefs.findById(k).map(CardXref::toRecord),
                xrefs::existsById, r -> {
                    throw new UnsupportedOperationException(dd + " is opened INPUT");
                }, r -> false, () -> {
                }, CardXrefRecord::cardNum);
    }

    private static KeyedDataset<Long, AccountRecord> account(JobParameters parameters, RecordEncoding encoding,
                                                             AccountRepository accounts, KeyedDataset.Mode mode) {
        String dd = Cbtrn02c.ACCTFILE;
        if (!DdParameters.isTable(parameters, dd)) {
            return KeyedDataset.file(dd, DdParameters.path(parameters, dd), mode, AccountRecord.MAPPER,
                    AccountRecord::acctId, k -> String.format(Locale.ROOT, "%011d", k), encoding);
        }
        return KeyedDataset.table(dd, mode, k -> accounts.findById(k).map(Account::toRecord), accounts::existsById,
                r -> accounts.save(Account.from(r)),
                r -> accounts.findById(r.acctId()).map(a -> {
                    a.update(r);
                    return true;
                }).orElse(false), () -> {
                }, AccountRecord::acctId);
    }

    private static KeyedDataset<TranCatBalanceId, TranCatBalanceRecord> balances(
            JobParameters parameters, RecordEncoding encoding, TranCatBalanceRepository balances) {
        String dd = Cbtrn02c.TCATBALF;
        Function<TranCatBalanceRecord, TranCatBalanceId> key =
                r -> new TranCatBalanceId(r.acctId(), r.tranTypeCd(), r.tranCatCd());
        if (!DdParameters.isTable(parameters, dd)) {
            return KeyedDataset.file(dd, DdParameters.path(parameters, dd), KeyedDataset.Mode.I_O,
                    TranCatBalanceRecord.MAPPER, key, Cbtrn02c::tcatKey, encoding);
        }
        return KeyedDataset.table(dd, KeyedDataset.Mode.I_O,
                k -> balances.findById(k).map(TranCatBalance::toRecord), balances::existsById,
                r -> balances.save(TranCatBalance.from(r)),
                r -> balances.findById(key.apply(r)).map(b -> {
                    b.update(r);
                    return true;
                }).orElse(false), () -> {
                }, key);
    }

    private static KeyedDataset<String, TransactionRecord> transactions(JobParameters parameters,
                                                                        RecordEncoding encoding,
                                                                        TransactionRepository transactions,
                                                                        Cbtrn02c.UnitOfWork unit) {
        String dd = Cbtrn02c.TRANFILE;
        if (!DdParameters.isTable(parameters, dd)) {
            return KeyedDataset.file(dd, DdParameters.path(parameters, dd), KeyedDataset.Mode.OUTPUT,
                    TransactionRecord.MAPPER, TransactionRecord::tranId, k -> pad(k, 16), encoding);
        }
        return KeyedDataset.table(dd, KeyedDataset.Mode.OUTPUT,
                k -> transactions.findById(k).map(Transaction::toRecord), transactions::existsById,
                r -> transactions.save(Transaction.from(r)), r -> false,
                () -> unit.run(transactions::deleteAllInBatch), TransactionRecord::tranId);
    }

    private static <E, K, D extends Record> KsdsInput input(JobParameters parameters, String ddname,
                                                            RecordEncoding encoding, CopybookRecordMapper<D> mapper,
                                                            K lowValues, BiFunction<K, Limit, List<E>> after,
                                                            Function<E, K> key, Function<E, D> toData) {
        if (!DdParameters.isTable(parameters, ddname)) {
            return KsdsInput.file(ddname, DdParameters.path(parameters, ddname), mapper.layout(), encoding);
        }
        return KsdsInput.table(ddname, lowValues, after, key, e -> mapper.toRecord(toData.apply(e), encoding),
                PAGE_SIZE);
    }

    private static Job job(String name, String stepName, JobRepository jobRepository,
                           PlatformTransactionManager transactionManager, BatchOutputProperties output,
                           ProgramStep program) {
        return job(name, stepName, jobRepository, transactionManager, output, true, program);
    }

    private static Job job(String name, String stepName, JobRepository jobRepository,
                           PlatformTransactionManager transactionManager, BatchOutputProperties output,
                           boolean restartable, ProgramStep program) {
        JobBuilder builder = new JobBuilder(name, jobRepository);
        if (!restartable) {
            builder.preventRestart();
        }
        return builder
                .start(new StepBuilder(stepName, jobRepository).tasklet((contribution, chunk) -> {
                    StepExecution step = chunk.getStepContext().getStepExecution();
                    JobParameters parameters = step.getJobParameters();
                    RecordEncoding encoding = DdParameters.encoding(parameters);
                    Path sysoutPath = DdParameters.sysout(parameters, output.outputDir(), name,
                            step.getJobExecutionId());
                    step.getExecutionContext().putString(SYSOUT_CONTEXT_KEY,
                            sysoutPath.toAbsolutePath().normalize().toString());
                    try (Sysout sysout = Sysout.open(sysoutPath)) {
                        ReturnCode.set(step, program.run(step, contribution, parameters, sysout, encoding));
                    }
                    return RepeatStatus.FINISHED;
                }, transactionManager).build())
                .build();
    }
}
