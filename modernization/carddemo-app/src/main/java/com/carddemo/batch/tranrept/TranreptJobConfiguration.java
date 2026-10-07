package com.carddemo.batch.tranrept;

import com.carddemo.batch.BaselineRunProperties;
import com.carddemo.batch.harness.BatchCommandLine;
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
import com.carddemo.common.file.RecordFiles;
import com.carddemo.transaction.TransactionCategory;
import com.carddemo.transaction.TransactionCategoryId;
import com.carddemo.transaction.TransactionCategoryRecord;
import com.carddemo.transaction.TransactionCategoryRepository;
import com.carddemo.transaction.TransactionRecord;
import com.carddemo.transaction.TransactionRepository;
import com.carddemo.transaction.TransactionType;
import com.carddemo.transaction.TransactionTypeRecord;
import com.carddemo.transaction.TransactionTypeRepository;
import java.nio.file.Path;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.PlatformTransactionManager;

/**
 * TRANREPT ({@code app/jcl/TRANREPT.jcl}, PROC {@code app/proc/TRANREPT.prc}) as a harness job stream (ADR-0015).
 * The JCL names two steps {@code STEP05R}; the stream uses the baseline's names:
 * <ul>
 * <li>{@code STEP05} {@code reproc}: TRANSACT (table, or {@code --STEP05.FILEIN=<unload>}) to
 * {@code TRANSACT.BKUP(+1)}.</li>
 * <li>{@code STEP10} {@code tranrept-sort}: the SORT {@code INCLUDE COND=(TRAN-PROC-DT,GE,PARM-START-DATE,AND,
 * TRAN-PROC-DT,LE,PARM-END-DATE)} / {@code SORT FIELDS=(TRAN-CARD-NUM,A)} becomes a date-range query on
 * {@code transaction} ({@link TransactionRepository#findByProcDateWindow}); {@code --SORTIN=<unload>} filters a file
 * instead. Output {@code TRANSACT.DALY(+1)}.</li>
 * <li>{@code STEP15} {@code cbtrn03c}: {@link Cbtrn03c} reads TRANSACT.DALY(0) and writes {@code TRANREPT(+1)}.</li>
 * </ul>
 * The window is {@code --PARM-START-DATE} / {@code --PARM-END-DATE}, else {@code carddemo.baseline.tranrept-*-date}
 * (golden profile: 2022-01-01..2022-07-06), else the run date for both; DATEPARM defaults to a record built from the
 * same window ({@code yyyy-mm-dd yyyy-mm-dd}, LRECL 80), as the baseline generates it.
 */
@Configuration(proxyBeanMethods = false)
public class TranreptJobConfiguration {

    private static final Logger log = LoggerFactory.getLogger(TranreptJobConfiguration.class);

    public static final String TRANREPT = "tranrept";
    public static final String TRANREPT_SORT = "tranrept-sort";
    public static final String CBTRN03C_JOB = "cbtrn03c";
    public static final String STEP05 = "STEP05";
    public static final String STEP10 = "STEP10";
    public static final String STEP15 = "STEP15";
    public static final String PARM_START_DATE = "PARM-START-DATE";
    public static final String PARM_END_DATE = "PARM-END-DATE";
    public static final String TRANSACT_DALY = "TRANSACT.DALY";
    public static final String TRANREPT_GDG = "TRANREPT";
    public static final int DATEPARM_LRECL = 80;

    /** TRAN-PROC-DT: bytes 305-314 of the TRANSACT record (TRAN-PROC-TS(1:10)). */
    static final int PROC_DT_OFFSET = 304;

    @Bean
    JobStream tranreptStream() {
        return new JobStream() {
            @Override
            public String name() {
                return TRANREPT;
            }

            @Override
            public JobChain chain(BatchJobLauncher launcher, JobParameters parameters) {
                return new JobChain(launcher)
                        .step(STEP05, HousekeepingJobConfiguration.REPROC, HousekeepingJobConfiguration.with(
                                JobStream.forStep(parameters, STEP05),
                                Map.of(HousekeepingJobConfiguration.DATASET, Dataset.TRANSACT.name(),
                                        HousekeepingJobConfiguration.GDG, HousekeepingJobConfiguration.TRANSACT_BKUP)),
                                null)
                        .step(STEP10, TRANREPT_SORT, JobStream.forStep(parameters, STEP10), null)
                        .step(STEP15, CBTRN03C_JOB, JobStream.forStep(parameters, STEP15), null);
            }
        };
    }

    @Bean
    Job tranreptSortJob(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                        SequentialDatasets datasets, BaselineRunProperties baseline,
                        TransactionRepository transactions) {
        return HousekeepingJobConfiguration.utilityJob(TRANREPT_SORT, jobRepository, transactionManager, datasets,
                (step, sysprint) -> {
                    JobParameters parameters = step.getJobParameters();
                    RecordEncoding encoding = DdParameters.encoding(parameters);
                    String[] window = window(parameters, baseline);
                    String sortin = HousekeepingJobConfiguration.SORTIN;
                    List<FixedWidthRecord> in;
                    long inCount;
                    if (DdParameters.isTable(parameters, sortin)) {
                        in = transactions.findByProcDateWindow(window[0], window[1]).stream()
                                .map(t -> TransactionRecord.MAPPER.toRecord(t.toRecord(), encoding)).toList();
                        inCount = transactions.count();
                    } else {
                        in = datasets.ksds(Dataset.TRANSACT, parameters, sortin, encoding);
                        inCount = in.size();
                    }
                    List<FixedWidthRecord> sorted = extract(in, window[0], window[1], encoding);
                    String file = datasets.write(step, HousekeepingJobConfiguration.SORTOUT, TRANSACT_DALY, sorted,
                            encoding);
                    sysprint.display(Dfsort.summary(inCount, sorted.size()));
                    log.info("{}: INCLUDE TRAN-PROC-DT {}..{} SORT FIELDS=(263,16,ZD,A): {} in -> {} selected"
                                    + " -> TRANSACT.DALY {}", TRANREPT_SORT, window[0], window[1], inCount,
                            sorted.size(), file);
                    return new HousekeepingJobConfiguration.Counts(inCount, sorted.size(), ReturnCode.OK);
                });
    }

    @Bean
    Job cbtrn03cJob(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                    SequentialDatasets datasets, BaselineRunProperties baseline, CardXrefRepository xrefs,
                    TransactionTypeRepository types, TransactionCategoryRepository categories) {
        return new JobBuilder(CBTRN03C_JOB, jobRepository)
                .start(new StepBuilder(STEP15, jobRepository).tasklet((contribution, chunk) -> {
                    StepExecution step = chunk.getStepContext().getStepExecution();
                    JobParameters parameters = step.getJobParameters();
                    RecordEncoding encoding = DdParameters.encoding(parameters);
                    BufferedSink report = new BufferedSink(Cbtrn03c.TRANREPT);
                    Cbtrn03c.Result result;
                    try (Sysout sysout = datasets.sysout(step, CBTRN03C_JOB)) {
                        result = new Cbtrn03c(
                                KsdsInput.records(Cbtrn03c.TRANFILE, () -> datasets.sequential(parameters,
                                        Cbtrn03c.TRANFILE, TRANSACT_DALY, TransactionRecord.MAPPER.layout(),
                                        encoding)),
                                xref(parameters, encoding, xrefs), trantype(parameters, encoding, types),
                                trancatg(parameters, encoding, categories), dateparm(parameters, encoding, baseline),
                                report, encoding, sysout).run();
                    }
                    String file = datasets.write(step, Cbtrn03c.TRANREPT, TRANREPT_GDG, report.records(), encoding);
                    log.info("{}: {} TRANFILE records, {} reported, {} report lines -> TRANREPT {}", CBTRN03C_JOB,
                            result.read(), result.reported(), result.lines(), file);
                    for (long i = 0; i < result.read(); i++) {
                        contribution.incrementReadCount();
                    }
                    contribution.incrementWriteCount(result.lines());
                    ReturnCode.set(step, result.returnCode());
                    return RepeatStatus.FINISHED;
                }, transactionManager).build())
                .build();
    }

    /** The DATEPARM / SYMNAMES window {@code [start, end]} as {@code yyyy-mm-dd} strings. */
    static String[] window(JobParameters parameters, BaselineRunProperties baseline) {
        LocalDate runDate = parameters.getLocalDate(BatchCommandLine.RUN_DATE);
        String start = pick(parameters.getString(PARM_START_DATE),
                baseline == null ? null : baseline.tranreptStartDate(), runDate);
        String end = pick(parameters.getString(PARM_END_DATE),
                baseline == null ? null : baseline.tranreptEndDate(), runDate);
        if (start == null || end == null) {
            throw new ReturnCodeException(ReturnCode.TERMINAL,
                    TRANREPT + " needs --PARM-START-DATE / --PARM-END-DATE or --run-date");
        }
        return new String[] {start, end};
    }

    private static String pick(String parameter, LocalDate configured, LocalDate runDate) {
        if (parameter != null && !parameter.isBlank()) {
            return parameter;
        }
        if (configured != null) {
            return configured.toString();
        }
        return runDate == null ? null : runDate.toString();
    }

    /** TRANREPT STEP10: the records with TRAN-PROC-DT in {@code [start, end]}, stable-sorted by card number. */
    public static List<FixedWidthRecord> extract(List<FixedWidthRecord> records, String start, String end,
                                                 RecordEncoding encoding) {
        byte[] from = encoding.encode(start);
        byte[] to = encoding.encode(end);
        return Dfsort.sort(records.stream().filter(r -> inWindow(r, from, to)).toList(), Dfsort.zd(263, 16));
    }

    static boolean inWindow(FixedWidthRecord record, byte[] start, byte[] end) {
        byte[] image = record.bytes();
        int from = PROC_DT_OFFSET;
        int to = PROC_DT_OFFSET + 10;
        return Arrays.compareUnsigned(image, from, to, start, 0, start.length) >= 0
                && Arrays.compareUnsigned(image, from, to, end, 0, end.length) <= 0;
    }

    static KsdsInput dateparm(JobParameters parameters, RecordEncoding encoding, BaselineRunProperties baseline) {
        String dd = Cbtrn03c.DATEPARM;
        if (DdParameters.isTable(parameters, dd)) {
            String[] window = window(parameters, baseline);
            String text = Cbtrn03c.pad(window[0] + " " + window[1], DATEPARM_LRECL);
            return KsdsInput.records(dd, () -> List.of(new FixedWidthRecord(encoding.encode(text), encoding)));
        }
        Path path = DdParameters.path(parameters, dd);
        return KsdsInput.records(dd, () -> {
            byte[] data = RecordFiles.read(dd, path);
            List<FixedWidthRecord> records = new ArrayList<>();
            if (encoding == RecordEncoding.EBCDIC) {
                for (int i = 0; i + DATEPARM_LRECL <= data.length; i += DATEPARM_LRECL) {
                    records.add(new FixedWidthRecord(Arrays.copyOfRange(data, i, i + DATEPARM_LRECL), encoding));
                }
            } else if (data.length > 0) {
                for (String line : new String(data, encoding.charset()).split("\r?\n")) {
                    records.add(new FixedWidthRecord(encoding.encode(Cbtrn03c.pad(line, DATEPARM_LRECL)),
                            encoding));
                }
            }
            return records;
        });
    }

    private static KeyedDataset<String, CardXrefRecord> xref(JobParameters parameters, RecordEncoding encoding,
                                                             CardXrefRepository xrefs) {
        String dd = Cbtrn03c.CARDXREF;
        if (!DdParameters.isTable(parameters, dd)) {
            return KeyedDataset.file(dd, DdParameters.path(parameters, dd), KeyedDataset.Mode.INPUT,
                    CardXrefRecord.MAPPER, CardXrefRecord::cardNum, k -> Cbtrn03c.pad(k, 16), encoding);
        }
        return KeyedDataset.table(dd, KeyedDataset.Mode.INPUT, k -> xrefs.findById(k).map(CardXref::toRecord),
                xrefs::existsById, r -> {
                    throw new UnsupportedOperationException(dd + " is opened INPUT");
                }, r -> false, () -> {
                }, CardXrefRecord::cardNum);
    }

    private static KeyedDataset<String, TransactionTypeRecord> trantype(JobParameters parameters,
                                                                        RecordEncoding encoding,
                                                                        TransactionTypeRepository types) {
        String dd = Cbtrn03c.TRANTYPE;
        if (!DdParameters.isTable(parameters, dd)) {
            return KeyedDataset.file(dd, DdParameters.path(parameters, dd), KeyedDataset.Mode.INPUT,
                    TransactionTypeRecord.MAPPER, TransactionTypeRecord::tranTypeCd, k -> Cbtrn03c.pad(k, 2),
                    encoding);
        }
        return KeyedDataset.table(dd, KeyedDataset.Mode.INPUT, k -> types.findById(k).map(TransactionType::toRecord),
                types::existsById, r -> {
                    throw new UnsupportedOperationException(dd + " is opened INPUT");
                }, r -> false, () -> {
                }, TransactionTypeRecord::tranTypeCd);
    }

    private static KeyedDataset<TransactionCategoryId, TransactionCategoryRecord> trancatg(
            JobParameters parameters, RecordEncoding encoding, TransactionCategoryRepository categories) {
        String dd = Cbtrn03c.TRANCATG;
        if (!DdParameters.isTable(parameters, dd)) {
            return KeyedDataset.file(dd, DdParameters.path(parameters, dd), KeyedDataset.Mode.INPUT,
                    TransactionCategoryRecord.MAPPER, r -> new TransactionCategoryId(r.tranTypeCd(), r.tranCatCd()),
                    Cbtrn03c::categoryKey, encoding);
        }
        return KeyedDataset.table(dd, KeyedDataset.Mode.INPUT,
                k -> categories.findById(k).map(TransactionCategory::toRecord), categories::existsById, r -> {
                    throw new UnsupportedOperationException(dd + " is opened INPUT");
                }, r -> false, () -> {
                }, r -> new TransactionCategoryId(r.tranTypeCd(), r.tranCatCd()));
    }
}
