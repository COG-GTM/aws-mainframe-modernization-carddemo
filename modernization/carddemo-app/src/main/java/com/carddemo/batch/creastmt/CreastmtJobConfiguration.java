package com.carddemo.batch.creastmt;

import com.carddemo.account.Account;
import com.carddemo.account.AccountRecord;
import com.carddemo.account.AccountRepository;
import com.carddemo.batch.harness.BatchJobLauncher;
import com.carddemo.batch.harness.BufferedSink;
import com.carddemo.batch.harness.DdParameters;
import com.carddemo.batch.harness.JobChain;
import com.carddemo.batch.harness.JobStream;
import com.carddemo.batch.harness.KeyedDataset;
import com.carddemo.batch.harness.KsdsInput;
import com.carddemo.batch.harness.ReturnCode;
import com.carddemo.batch.harness.ReturnCodeException;
import com.carddemo.batch.harness.Sysout;
import com.carddemo.batch.housekeeping.Dfsort;
import com.carddemo.batch.housekeeping.HousekeepingJobConfiguration;
import com.carddemo.batch.housekeeping.SequentialDatasets;
import com.carddemo.batch.load.VsamDatasetLoader.Dataset;
import com.carddemo.card.CardXref;
import com.carddemo.card.CardXrefRecord;
import com.carddemo.card.CardXrefRepository;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.codec.RecordLayout;
import com.carddemo.customer.Customer;
import com.carddemo.customer.CustomerRecord;
import com.carddemo.customer.CustomerRepository;
import com.carddemo.transaction.TransactionRecord;
import com.carddemo.transaction.TransactionRepository;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.domain.Limit;
import org.springframework.transaction.PlatformTransactionManager;

/**
 * CREASTMT ({@code app/jcl/CREASTMT.JCL}) as a harness job stream (ADR-0015):
 * <ul>
 * <li>{@code STEP010} {@code creastmt-sort}: {@code SORT FIELDS=(263,16,CH,A,1,16,CH,A)} +
 * {@code OUTREC FIELDS=(1:263,16,17:1,262,279:279,50)} over TRANSACT (table: the query
 * {@link TransactionRepository#findAllForStatements}; {@code --STEP010.SORTIN=<unload>}: the file) to
 * {@code TRXFL.SEQ(+1)}, LRECL 350.</li>
 * <li>{@code STEP020} {@code trxfl-repro}: DEFINE CLUSTER TRXFL {@code KEYS(32 0)} + REPRO of the generation STEP010
 * wrote, i.e. the 32-byte keys (card + transaction id) must ascend without duplicates (IDC3316I / IDC3314I, RC 12);
 * the cluster is written as the dated generation {@code TRXFL(+1)}.</li>
 * <li>{@code STEP040} {@code cbstm03a}: {@link Cbstm03a} reads TRNXFILE (the TRXFL generation STEP020 wrote, or
 * {@code --TRNXFILE=<file>}), XREFFILE / CUSTFILE / ACCTFILE (tables, or {@code --<DD>=<unload>}) through
 * {@link Cbstm03b} and writes {@code STATEMNT.PS(+1)} (STMTFILE, LRECL 80) and {@code STATEMNT.HTML(+1)} (HTMLFILE,
 * LRECL 100), catalogued only when the program ends normally ({@code DISP=(NEW,CATLG,DELETE)}).</li>
 * </ul>
 * DELDEF01 (DELETE/DEFINE TRXFL) and STEP030 (IEFBR14 deleting the previous statements) have no Java step: every
 * run writes new dated generations (ADR-0012), so there is nothing to delete or pre-allocate
 * ({@code docs/modernization/06-scheduling.md}).
 */
@Configuration(proxyBeanMethods = false)
public class CreastmtJobConfiguration {

    private static final Logger log = LoggerFactory.getLogger(CreastmtJobConfiguration.class);

    public static final String CREASTMT = "creastmt";
    public static final String CREASTMT_SORT = "creastmt-sort";
    public static final String TRXFL_REPRO = "trxfl-repro";
    public static final String CBSTM03A_JOB = "cbstm03a";
    public static final String STEP010 = "STEP010";
    public static final String STEP020 = "STEP020";
    public static final String STEP040 = "STEP040";
    public static final String TRXFL_SEQ = "TRXFL.SEQ";
    public static final String TRXFL = "TRXFL";
    public static final String STATEMNT_PS = "STATEMNT.PS";
    public static final String STATEMNT_HTML = "STATEMNT.HTML";
    public static final int TRXFL_LRECL = 350;
    public static final int TRXFL_KEY_LENGTH = 32;
    static final int TABLE_PAGE_SIZE = 500;

    static final String FUNCTION_COMPLETED = "IDC0001I FUNCTION COMPLETED, HIGHEST CONDITION CODE WAS ";

    @Bean
    JobStream creastmtStream() {
        return new JobStream() {
            @Override
            public String name() {
                return CREASTMT;
            }

            @Override
            public JobChain chain(BatchJobLauncher launcher, JobParameters parameters) {
                return new JobChain(launcher)
                        .step(STEP010, CREASTMT_SORT, JobStream.forStep(parameters, STEP010), null)
                        .step(STEP020, TRXFL_REPRO, SequentialDatasets.bind(JobStream.forStep(parameters, STEP020),
                                HousekeepingJobConfiguration.INFILE, STEP010, HousekeepingJobConfiguration.SORTOUT),
                                "(0,NE)")
                        .step(STEP040, CBSTM03A_JOB, SequentialDatasets.bind(JobStream.forStep(parameters, STEP040),
                                Cbstm03b.TRNXFILE, STEP020, HousekeepingJobConfiguration.OUTFILE), "(0,NE)");
            }
        };
    }

    @Bean
    Job creastmtSortJob(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                        SequentialDatasets datasets, TransactionRepository transactions) {
        return HousekeepingJobConfiguration.utilityJob(CREASTMT_SORT, jobRepository, transactionManager, datasets,
                (step, sysprint) -> {
                    JobParameters parameters = step.getJobParameters();
                    RecordEncoding encoding = DdParameters.encoding(parameters);
                    String sortin = HousekeepingJobConfiguration.SORTIN;
                    List<FixedWidthRecord> sorted;
                    if (DdParameters.isTable(parameters, sortin)) {
                        // the query's COLLATE "C" order is ASCII byte order; re-sort the encoded records so EBCDIC
                        // runs get EBCDIC CH order, as DFSORT would
                        sorted = sort(transactions.findAllForStatements().stream()
                                .map(t -> TransactionRecord.MAPPER.toRecord(t.toRecord(), encoding)).toList());
                    } else {
                        sorted = sort(datasets.ksds(Dataset.TRANSACT, parameters, sortin, encoding));
                    }
                    List<FixedWidthRecord> trxfl = sorted.stream().map(r -> outrec(r, encoding)).toList();
                    String file = datasets.write(step, HousekeepingJobConfiguration.SORTOUT, TRXFL_SEQ, trxfl,
                            encoding);
                    sysprint.display(Dfsort.summary(sorted.size(), trxfl.size()));
                    log.info("{}: SORT FIELDS=(263,16,CH,A,1,16,CH,A) OUTREC FIELDS=(1:263,16,17:1,262,279:279,50):"
                            + " {} records -> TRXFL.SEQ {}", CREASTMT_SORT, trxfl.size(), file);
                    return new HousekeepingJobConfiguration.Counts(sorted.size(), trxfl.size(), ReturnCode.OK);
                });
    }

    @Bean
    Job trxflReproJob(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                      SequentialDatasets datasets) {
        return HousekeepingJobConfiguration.utilityJob(TRXFL_REPRO, jobRepository, transactionManager, datasets,
                (step, sysprint) -> {
                    JobParameters parameters = step.getJobParameters();
                    RecordEncoding encoding = DdParameters.encoding(parameters);
                    List<FixedWidthRecord> records = datasets.sequential(parameters,
                            HousekeepingJobConfiguration.INFILE, TRXFL_SEQ, Cbstm03a.TRNX_LAYOUT, encoding);
                    String error = keyError(records);
                    if (error != null) {
                        sysprint.display(error);
                        sysprint.display(FUNCTION_COMPLETED + ReturnCode.SEVERE.code());
                        throw new ReturnCodeException(ReturnCode.SEVERE, error);
                    }
                    String file = datasets.write(step, HousekeepingJobConfiguration.OUTFILE, TRXFL, records,
                            encoding);
                    sysprint.display("IDC0005I NUMBER OF RECORDS PROCESSED WAS " + records.size());
                    sysprint.display(FUNCTION_COMPLETED + 0);
                    log.info("{}: DEFINE CLUSTER TRXFL KEYS(32 0) RECSZ(350) + REPRO: {} records -> TRXFL {}",
                            TRXFL_REPRO, records.size(), file);
                    return new HousekeepingJobConfiguration.Counts(records.size(), records.size(), ReturnCode.OK);
                });
    }

    @Bean
    Job cbstm03aJob(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                    SequentialDatasets datasets, CardXrefRepository xrefs, CustomerRepository customers,
                    AccountRepository accounts,
                    @Value("${carddemo.batch.creastmt.html-escape:false}") boolean htmlEscape) {
        return new JobBuilder(CBSTM03A_JOB, jobRepository)
                .start(new StepBuilder(STEP040, jobRepository).tasklet((contribution, chunk) -> {
                    StepExecution step = chunk.getStepContext().getStepExecution();
                    JobParameters parameters = step.getJobParameters();
                    RecordEncoding encoding = DdParameters.encoding(parameters);
                    BufferedSink stmt = new BufferedSink(Cbstm03a.STMTFILE);
                    BufferedSink html = new BufferedSink(Cbstm03a.HTMLFILE);
                    Cbstm03a.Result result;
                    try (Sysout sysout = datasets.sysout(step, CBSTM03A_JOB)) {
                        Cbstm03b files = new Cbstm03b(
                                KsdsInput.records(Cbstm03b.TRNXFILE, () -> datasets.sequential(parameters,
                                        Cbstm03b.TRNXFILE, TRXFL, Cbstm03a.TRNX_LAYOUT, encoding)),
                                xreffile(parameters, encoding, xrefs), custfile(parameters, encoding, customers),
                                acctfile(parameters, encoding, accounts), encoding);
                        result = new Cbstm03a(files, stmt, html, encoding, sysout, "CREASTMT", STEP040, htmlEscape)
                                .run();
                    }
                    String stmtFile = datasets.write(step, Cbstm03a.STMTFILE, STATEMNT_PS, stmt.records(), encoding);
                    String htmlFile = datasets.write(step, Cbstm03a.HTMLFILE, STATEMNT_HTML, html.records(),
                            encoding);
                    log.info("{}: {} TRNXFILE records, {} statements, {} transaction lines -> STATEMNT.PS {} ({}),"
                                    + " STATEMNT.HTML {} ({})", CBSTM03A_JOB, result.read(), result.statements(),
                            result.transactions(), stmtFile, result.stmtLines(), htmlFile, result.htmlLines());
                    for (long i = 0; i < result.read(); i++) {
                        contribution.incrementReadCount();
                    }
                    contribution.incrementWriteCount(result.stmtLines() + result.htmlLines());
                    ReturnCode.set(step, result.returnCode());
                    return RepeatStatus.FINISHED;
                }, transactionManager).build())
                .build();
    }

    /** STEP010 on a file: {@code SORT FIELDS=(263,16,CH,A,1,16,CH,A)} (TRAN-CARD-NUM, then TRAN-ID). */
    public static List<FixedWidthRecord> sort(List<FixedWidthRecord> transact) {
        return Dfsort.sort(transact, Dfsort.ch(263, 16).thenComparing(Dfsort.ch(1, 16)));
    }

    /** {@code OUTREC FIELDS=(1:263,16,17:1,262,279:279,50)}, blank-padded to the SORTOUT LRECL 350. */
    public static FixedWidthRecord outrec(FixedWidthRecord transact, RecordEncoding encoding) {
        byte[] in = transact.bytes();
        byte[] out = new byte[TRXFL_LRECL];
        Arrays.fill(out, encoding.space());
        System.arraycopy(in, 262, out, 0, 16);
        System.arraycopy(in, 0, out, 16, 262);
        System.arraycopy(in, 278, out, 278, 50);
        return new FixedWidthRecord(Cbstm03a.TRNX_LAYOUT, out, encoding);
    }

    /** REPRO into {@code KEYS(32 0)}: the first key that is not greater than the one before, as an IDCAMS message. */
    static String keyError(List<FixedWidthRecord> records) {
        byte[] previous = null;
        for (FixedWidthRecord record : records) {
            byte[] key = Arrays.copyOf(record.bytes(), TRXFL_KEY_LENGTH);
            if (previous != null && Arrays.compareUnsigned(key, previous) <= 0) {
                String text = record.text().substring(0, TRXFL_KEY_LENGTH);
                return Arrays.equals(key, previous) ? "IDC3316I DUPLICATE RECORD - KEY " + text
                        : "IDC3314I RECORD OUT OF SEQUENCE - KEY " + text;
            }
            previous = key;
        }
        return null;
    }

    static KsdsInput xreffile(JobParameters parameters, RecordEncoding encoding, CardXrefRepository xrefs) {
        String dd = Cbstm03b.XREFFILE;
        RecordLayout layout = CardXrefRecord.MAPPER.layout();
        if (!DdParameters.isTable(parameters, dd)) {
            return KsdsInput.file(dd, DdParameters.path(parameters, dd), layout, encoding);
        }
        return KsdsInput.table(dd, "", (String last, Limit limit) ->
                        xrefs.findByCardNumGreaterThanOrderByCardNumAsc(last, limit), CardXref::getCardNum,
                (CardXref x) -> CardXrefRecord.MAPPER.toRecord(x.toRecord(), encoding), TABLE_PAGE_SIZE);
    }

    static KeyedDataset<Integer, CustomerRecord> custfile(JobParameters parameters, RecordEncoding encoding,
                                                         CustomerRepository customers) {
        String dd = Cbstm03b.CUSTFILE;
        if (!DdParameters.isTable(parameters, dd)) {
            return KeyedDataset.file(dd, DdParameters.path(parameters, dd), KeyedDataset.Mode.INPUT,
                    CustomerRecord.MAPPER, CustomerRecord::custId, k -> String.format("%09d", k), encoding);
        }
        return KeyedDataset.table(dd, KeyedDataset.Mode.INPUT, k -> customers.findById(k).map(Customer::toRecord),
                customers::existsById, r -> {
                    throw new UnsupportedOperationException(dd + " is opened INPUT");
                }, r -> false, () -> {
                }, CustomerRecord::custId);
    }

    static KeyedDataset<Long, AccountRecord> acctfile(JobParameters parameters, RecordEncoding encoding,
                                                     AccountRepository accounts) {
        String dd = Cbstm03b.ACCTFILE;
        if (!DdParameters.isTable(parameters, dd)) {
            return KeyedDataset.file(dd, DdParameters.path(parameters, dd), KeyedDataset.Mode.INPUT,
                    AccountRecord.MAPPER, AccountRecord::acctId, k -> String.format("%011d", k), encoding);
        }
        return KeyedDataset.table(dd, KeyedDataset.Mode.INPUT, k -> accounts.findById(k).map(Account::toRecord),
                accounts::existsById, r -> {
                    throw new UnsupportedOperationException(dd + " is opened INPUT");
                }, r -> false, () -> {
                }, AccountRecord::acctId);
    }

    /** The records a file-mode run of STEP010 + STEP020 builds from a TRANSACT unload (tests, tools). */
    public static List<FixedWidthRecord> trxfl(List<FixedWidthRecord> transact, RecordEncoding encoding) {
        List<FixedWidthRecord> out = new ArrayList<>();
        for (FixedWidthRecord record : sort(transact)) {
            out.add(outrec(record, encoding));
        }
        return out;
    }
}
