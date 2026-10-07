package com.carddemo.batch.housekeeping;

import com.carddemo.batch.harness.BatchJobLauncher;
import com.carddemo.batch.harness.DdParameters;
import com.carddemo.batch.harness.JobChain;
import com.carddemo.batch.harness.JobStream;
import com.carddemo.batch.harness.ReturnCode;
import com.carddemo.batch.harness.ReturnCodeException;
import com.carddemo.batch.harness.Sysout;
import com.carddemo.batch.load.LoadMode;
import com.carddemo.batch.load.LoadResult;
import com.carddemo.batch.load.VsamDatasetLoader;
import com.carddemo.batch.load.VsamDatasetLoader.Dataset;
import com.carddemo.common.AbendException;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.file.FileStatus;
import com.carddemo.common.file.FileStatusException;
import com.carddemo.common.file.RecordFiles;
import com.carddemo.transaction.TranCatBalanceRecord;
import com.carddemo.transaction.TransactionRecord;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HexFormat;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.PlatformTransactionManager;

/**
 * The housekeeping JCL as harness job streams (ADR-0015), with IDCAMS / DFSORT / IEFBR14 steps replaced by table
 * exports, resets and dated generations (ADR-0011, ADR-0012):
 * <ul>
 * <li>{@code tranbkp} ({@code TRANBKP.jcl}): {@code STEP05R} {@code reproc} TRANSACT to {@code TRANSACT.BKUP(+1)},
 * {@code STEP05} {@code idcams-delete} (DELETE CLUSTER: the table is emptied; {@code IF MAXCC LE 08 THEN SET MAXCC = 0}
 * makes it RC 0), {@code STEP10} {@code idcams-define} with {@code COND=(4,LT)} (DEFINE CLUSTER: the table exists, the
 * step checks it is empty).</li>
 * <li>{@code combtran} ({@code COMBTRAN.jcl}): {@code STEP05R} {@code combtran-sort} merges {@code TRANSACT.BKUP(0)}
 * (SORTIN) and {@code SYSTRAN(0)} (SORTIN02, the concatenated DD) by TRAN-ID into {@code TRANSACT.COMBINED(+1)},
 * {@code STEP10} {@code idcams-repro} loads it into TRANSACT.</li>
 * <li>{@code prtcatbl} ({@code PRTCATBL.jcl}): {@code DELDEF} (IEFBR14 delete of TCATBALF.REPT) is retired, every run
 * writes a new dated {@code TCATBALF.REPT}; {@code STEP05R} {@code reproc} TCATBALF to {@code TCATBALF.BKUP(+1)},
 * {@code STEP10R} {@code prtcatbl-sort} sorts and reformats it (41-byte OUTREC, see {@link #outrec}).</li>
 * </ul>
 * Each utility step writes its SYSPRINT messages (IDC0005I / ICE054I counts) to its SYSOUT.
 */
@Configuration(proxyBeanMethods = false)
public class HousekeepingJobConfiguration {

    private static final Logger log = LoggerFactory.getLogger(HousekeepingJobConfiguration.class);

    public static final String TRANBKP = "tranbkp";
    public static final String COMBTRAN = "combtran";
    public static final String PRTCATBL = "prtcatbl";
    public static final String REPROC = "reproc";
    public static final String IDCAMS_DELETE = "idcams-delete";
    public static final String IDCAMS_DEFINE = "idcams-define";
    public static final String IDCAMS_REPRO = "idcams-repro";
    public static final String COMBTRAN_SORT = "combtran-sort";
    public static final String PRTCATBL_SORT = "prtcatbl-sort";

    /** Job parameter: the KSDS a utility step works on ({@code TRANSACT} or {@code TCATBALF}). */
    public static final String DATASET = "DATASET";
    /** Job parameter: the GDG base of a utility step's generation DD. */
    public static final String GDG = "GDG";
    public static final String FILEIN = "FILEIN";
    public static final String FILEOUT = "FILEOUT";
    public static final String CLUSTER = "CLUSTER";
    public static final String INFILE = "INFILE";
    public static final String OUTFILE = "OUTFILE";
    public static final String SORTIN = "SORTIN";
    public static final String SORTIN02 = "SORTIN02";
    public static final String SORTOUT = "SORTOUT";

    public static final String TRANSACT_BKUP = "TRANSACT.BKUP";
    public static final String SYSTRAN = "SYSTRAN";
    public static final String TRANSACT_COMBINED = "TRANSACT.COMBINED";
    public static final String TCATBALF_BKUP = "TCATBALF.BKUP";
    public static final String TCATBALF_REPT = "TCATBALF.REPT";
    public static final int TCATBALF_REPT_LRECL = 41;

    static final String FUNCTION_COMPLETED = "IDC0001I FUNCTION COMPLETED, HIGHEST CONDITION CODE WAS ";

    /** Counts and RC of a utility step. */
    public record Counts(long read, long written, ReturnCode returnCode) {
    }

    @FunctionalInterface
    public interface UtilityStep {
        Counts run(StepExecution step, Sysout sysprint);
    }

    // --- streams ---

    @Bean
    JobStream tranbkpStream() {
        return stream(TRANBKP, (launcher, parameters) -> new JobChain(launcher)
                .step("STEP05R", REPROC, with(JobStream.forStep(parameters, "STEP05R"),
                        Map.of(DATASET, Dataset.TRANSACT.name(), GDG, TRANSACT_BKUP)), null)
                .step("STEP05", IDCAMS_DELETE, with(JobStream.forStep(parameters, "STEP05"),
                        Map.of(DATASET, Dataset.TRANSACT.name())), null)
                .step("STEP10", IDCAMS_DEFINE, with(JobStream.forStep(parameters, "STEP10"),
                        Map.of(DATASET, Dataset.TRANSACT.name())), "(4,LT)"));
    }

    @Bean
    JobStream combtranStream() {
        return stream(COMBTRAN, (launcher, parameters) -> new JobChain(launcher)
                .step("STEP05R", COMBTRAN_SORT, JobStream.forStep(parameters, "STEP05R"), null)
                .step("STEP10", IDCAMS_REPRO, with(JobStream.forStep(parameters, "STEP10"),
                        Map.of(DATASET, Dataset.TRANSACT.name(), GDG, TRANSACT_COMBINED)), null));
    }

    @Bean
    JobStream prtcatblStream() {
        return stream(PRTCATBL, (launcher, parameters) -> new JobChain(launcher)
                .step("STEP05R", REPROC, with(JobStream.forStep(parameters, "STEP05R"),
                        Map.of(DATASET, Dataset.TCATBALF.name(), GDG, TCATBALF_BKUP)), null)
                .step("STEP10R", PRTCATBL_SORT, JobStream.forStep(parameters, "STEP10R"), null));
    }

    // --- utility steps ---

    /** PROC {@code REPROC}: IDCAMS {@code REPRO INFILE(FILEIN) OUTFILE(FILEOUT)} of a KSDS to a backup generation. */
    @Bean
    Job reprocJob(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                  SequentialDatasets datasets) {
        return utilityJob(REPROC, jobRepository, transactionManager, datasets, (step, sysprint) -> {
            JobParameters parameters = step.getJobParameters();
            RecordEncoding encoding = DdParameters.encoding(parameters);
            Dataset dataset = dataset(parameters);
            List<FixedWidthRecord> records = datasets.ksds(dataset, parameters, FILEIN, encoding);
            String gdg = parameters.getString(GDG, dataset.name() + ".BKUP");
            String file = datasets.write(step, FILEOUT, gdg, records, encoding);
            sysprint.display("IDC0005I NUMBER OF RECORDS PROCESSED WAS " + records.size());
            sysprint.display(FUNCTION_COMPLETED + 0);
            log.info("{}: REPRO {} ({}) -> {} {}: {} records", REPROC, SequentialDatasets.cluster(dataset),
                    source(parameters, FILEIN), gdg, file, records.size());
            return new Counts(records.size(), records.size(), ReturnCode.OK);
        });
    }

    /** IDCAMS {@code DELETE <cluster> CLUSTER} + {@code IF MAXCC LE 08 THEN SET MAXCC = 0}: empties the dataset. */
    @Bean
    Job idcamsDeleteJob(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                        SequentialDatasets datasets, VsamDatasetLoader loader) {
        return utilityJob(IDCAMS_DELETE, jobRepository, transactionManager, datasets, (step, sysprint) -> {
            JobParameters parameters = step.getJobParameters();
            Dataset dataset = dataset(parameters);
            String cluster = SequentialDatasets.cluster(dataset);
            long deleted;
            boolean existed;
            if (!DdParameters.isTable(parameters, CLUSTER)) {
                Path path = DdParameters.path(parameters, CLUSTER);
                existed = Files.exists(path);
                deleted = existed ? dataset.read(path, DdParameters.encoding(parameters)).size() : 0;
                try {
                    Files.deleteIfExists(path);
                } catch (IOException e) {
                    throw new UncheckedIOException(e);
                }
            } else {
                existed = true;
                deleted = loader.count(dataset);
                loader.clear(List.of(dataset));
            }
            sysprint.display(existed ? "IDC0550I ENTRY (C) " + cluster + " DELETED"
                    : "IDC3012I ENTRY " + cluster + " NOT FOUND");
            sysprint.display(FUNCTION_COMPLETED + 0);
            log.info("{}: {} ({}) {}: {} records removed", IDCAMS_DELETE, cluster, source(parameters, CLUSTER),
                    existed ? "deleted" : "not found", deleted);
            return new Counts(deleted, 0, ReturnCode.OK);
        });
    }

    /** IDCAMS {@code DEFINE CLUSTER}: an empty unload file, or a check that the table is empty (RC 12 if not). */
    @Bean
    Job idcamsDefineJob(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                        SequentialDatasets datasets, VsamDatasetLoader loader) {
        return utilityJob(IDCAMS_DEFINE, jobRepository, transactionManager, datasets, (step, sysprint) -> {
            JobParameters parameters = step.getJobParameters();
            Dataset dataset = dataset(parameters);
            String cluster = SequentialDatasets.cluster(dataset);
            boolean exists;
            if (!DdParameters.isTable(parameters, CLUSTER)) {
                Path path = DdParameters.path(parameters, CLUSTER);
                try {
                    exists = Files.exists(path) && Files.size(path) > 0;
                    if (!exists) {
                        Path parent = path.toAbsolutePath().getParent();
                        if (parent != null) {
                            Files.createDirectories(parent);
                        }
                        Files.write(path, new byte[0]);
                    }
                } catch (IOException e) {
                    throw new UncheckedIOException(e);
                }
            } else {
                exists = loader.count(dataset) > 0;
            }
            if (exists) {
                sysprint.display("IDC3013I DUPLICATE DATA SET NAME " + cluster);
                sysprint.display(FUNCTION_COMPLETED + ReturnCode.SEVERE.code());
                throw new ReturnCodeException(ReturnCode.SEVERE, cluster + " is defined and not empty");
            }
            sysprint.display(FUNCTION_COMPLETED + 0);
            log.info("{}: {} ({}) defined empty", IDCAMS_DEFINE, cluster, source(parameters, CLUSTER));
            return new Counts(0, 0, ReturnCode.OK);
        });
    }

    /**
     * IDCAMS {@code REPRO INFILE(<generation>) OUTFILE(<cluster>)}: loads a sequential dataset in key order into the
     * KSDS. As a VSAM load, keys must ascend and must not be in the cluster yet; any violation ends the step with
     * RC 12 and loads nothing.
     */
    @Bean
    Job idcamsReproJob(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                       SequentialDatasets datasets, VsamDatasetLoader loader) {
        return utilityJob(IDCAMS_REPRO, jobRepository, transactionManager, datasets, (step, sysprint) -> {
            JobParameters parameters = step.getJobParameters();
            RecordEncoding encoding = DdParameters.encoding(parameters);
            Dataset dataset = dataset(parameters);
            String cluster = SequentialDatasets.cluster(dataset);
            String gdg = parameters.getString(GDG, dataset.name() + ".COMBINED");
            List<FixedWidthRecord> records = datasets.sequential(parameters, INFILE, gdg,
                    dataset.mapper().layout(), encoding);
            List<FixedWidthRecord> existing = datasets.ksds(dataset, parameters, OUTFILE, encoding);
            int keyLength = keyLength(dataset);
            Set<String> keys = new HashSet<>();
            existing.forEach(r -> keys.add(key(r, keyLength)));
            byte[] previous = null;
            for (FixedWidthRecord record : records) {
                byte[] key = Arrays.copyOf(record.bytes(), keyLength);
                String text = record.text().substring(0, keyLength);
                if (previous != null && Arrays.compareUnsigned(key, previous) <= 0) {
                    String message = Arrays.equals(key, previous) ? "IDC3316I DUPLICATE RECORD - KEY " + text
                            : "IDC3314I RECORD OUT OF SEQUENCE - KEY " + text;
                    throw severe(sysprint, message);
                }
                if (!keys.add(key(record, keyLength))) {
                    throw severe(sysprint, "IDC3316I DUPLICATE RECORD - KEY " + text);
                }
                previous = key;
            }
            if (!DdParameters.isTable(parameters, OUTFILE)) {
                List<FixedWidthRecord> merged = new ArrayList<>(existing);
                merged.addAll(records);
                merged.sort((a, b) -> Arrays.compareUnsigned(a.bytes(), 0, keyLength, b.bytes(), 0, keyLength));
                Path path = DdParameters.path(parameters, OUTFILE);
                if (encoding == RecordEncoding.EBCDIC) {
                    RecordFiles.writeFixed(OUTFILE, path, merged);
                } else {
                    RecordFiles.writeLines(OUTFILE, path, merged, false);
                }
            } else {
                LoadResult result = loader.load(dataset, records, LoadMode.UPSERT);
                if (!result.rejects().isEmpty()) {
                    LoadResult.Reject first = result.rejects().get(0);
                    throw severe(sysprint, "IDC3351I RECORD " + first.recordNumber() + " REJECTED: " + first.reason());
                }
            }
            sysprint.display("IDC0005I NUMBER OF RECORDS PROCESSED WAS " + records.size());
            sysprint.display(FUNCTION_COMPLETED + 0);
            log.info("{}: REPRO {} ({}) -> {} ({}): {} records loaded, {} already there", IDCAMS_REPRO, gdg,
                    datasets.inputPath(parameters, INFILE, gdg), cluster, source(parameters, OUTFILE), records.size(),
                    existing.size());
            return new Counts(records.size(), records.size(), ReturnCode.OK);
        });
    }

    /** COMBTRAN {@code STEP05R}: {@code SORT FIELDS=(TRAN-ID,A)} over TRANSACT.BKUP(0) + SYSTRAN(0). */
    @Bean
    Job combtranSortJob(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                        SequentialDatasets datasets) {
        return utilityJob(COMBTRAN_SORT, jobRepository, transactionManager, datasets, (step, sysprint) -> {
            JobParameters parameters = step.getJobParameters();
            RecordEncoding encoding = DdParameters.encoding(parameters);
            List<FixedWidthRecord> backup = datasets.sequential(parameters, SORTIN, TRANSACT_BKUP,
                    TransactionRecord.MAPPER.layout(), encoding);
            List<FixedWidthRecord> systran = datasets.sequential(parameters, SORTIN02, SYSTRAN,
                    TransactionRecord.MAPPER.layout(), encoding);
            List<FixedWidthRecord> all = new ArrayList<>(backup);
            all.addAll(systran);
            List<FixedWidthRecord> sorted = combine(backup, systran);
            String file = datasets.write(step, SORTOUT, TRANSACT_COMBINED, sorted, encoding);
            sysprint.display(Dfsort.summary(all.size(), sorted.size()));
            log.info("{}: SORT FIELDS=(1,16,CH,A) SORTIN {} ({}) + SORTIN02 {} ({}) -> TRANSACT.COMBINED {} ({})",
                    COMBTRAN_SORT, backup.size(), datasets.inputPath(parameters, SORTIN, TRANSACT_BKUP),
                    systran.size(), datasets.inputPath(parameters, SORTIN02, SYSTRAN), sorted.size(), file);
            return new Counts(all.size(), sorted.size(), ReturnCode.OK);
        });
    }

    /** PRTCATBL {@code STEP10R}: SORT by TRAN-CAT-KEY and OUTREC of TCATBALF.BKUP(+1) into TCATBALF.REPT. */
    @Bean
    Job prtcatblSortJob(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                        SequentialDatasets datasets) {
        return utilityJob(PRTCATBL_SORT, jobRepository, transactionManager, datasets, (step, sysprint) -> {
            JobParameters parameters = step.getJobParameters();
            RecordEncoding encoding = DdParameters.encoding(parameters);
            List<FixedWidthRecord> in = datasets.sequential(parameters, SORTIN, TCATBALF_BKUP,
                    TranCatBalanceRecord.MAPPER.layout(), encoding);
            List<FixedWidthRecord> out = categoryBalanceReport(in, encoding);
            String file = datasets.write(step, SORTOUT, TCATBALF_REPT, out, encoding);
            sysprint.display(Dfsort.summary(in.size(), out.size()));
            log.info("{}: SORT FIELDS=(1,11,ZD,A,12,2,CH,A,14,4,ZD,A) OUTREC: {} records -> TCATBALF.REPT {}",
                    PRTCATBL_SORT, out.size(), file);
            return new Counts(in.size(), out.size(), ReturnCode.OK);
        });
    }

    /** COMBTRAN STEP05R: SORTIN then SORTIN02, stable-sorted on TRAN-ID (1,16,CH). */
    public static List<FixedWidthRecord> combine(List<FixedWidthRecord> backup, List<FixedWidthRecord> systran) {
        List<FixedWidthRecord> all = new ArrayList<>(backup);
        all.addAll(systran);
        return Dfsort.sort(all, Dfsort.ch(1, 16));
    }

    /** PRTCATBL STEP10R: {@code SORT FIELDS=(1,11,ZD,A,12,2,CH,A,14,4,ZD,A)} and {@link #outrec}. */
    public static List<FixedWidthRecord> categoryBalanceReport(List<FixedWidthRecord> tcatbal,
                                                               RecordEncoding encoding) {
        return Dfsort.sort(tcatbal,
                        Dfsort.zd(1, 11).thenComparing(Dfsort.ch(12, 2)).thenComparing(Dfsort.zd(14, 4)))
                .stream().map(r -> new FixedWidthRecord(encoding.encode(outrec(r.text())), encoding)).toList();
    }

    /**
     * {@code OUTREC FIELDS=(TRANCAT-ACCT-ID,X,TRANCAT-TYPE-CD,X,TRANCAT-CD,X,TRAN-CAT-BAL,EDIT=(TTTTTTTTT.TT),9X)}:
     * 11 + 1 + 2 + 1 + 4 + 1 + 12 + 9 = 41 bytes (the JCL's SORTOUT LRECL=40 is one byte short; the baseline keeps 41).
     */
    static String outrec(String tcatbal) {
        return tcatbal.substring(0, 11) + " " + tcatbal.substring(11, 13) + " " + tcatbal.substring(13, 17) + " "
                + editTttttttttdtt(tcatbal.substring(17, 28)) + " ".repeat(9);
    }

    /** {@code ZD,EDIT=(TTTTTTTTT.TT)} of 11 zoned digits: every digit printed, the overpunched sign dropped. */
    static String editTttttttttdtt(String zoned) {
        char last = zoned.charAt(zoned.length() - 1);
        char digit;
        if (last == '{' || last == '}') {
            digit = '0';
        } else if (last >= 'A' && last <= 'I') {
            digit = (char) ('1' + last - 'A');
        } else if (last >= 'J' && last <= 'R') {
            digit = (char) ('1' + last - 'J');
        } else if (last >= 'p' && last <= 'y') {
            digit = (char) ('0' + last - 'p');
        } else {
            digit = last;
        }
        String digits = zoned.substring(0, zoned.length() - 1) + digit;
        return digits.substring(0, 9) + "." + digits.substring(9, 11);
    }

    // --- helpers ---

    /** A Spring Batch job of one tasklet step running {@code body}; I/O errors end it with RC 16. */
    public static Job utilityJob(String jobName, JobRepository jobRepository,
                                 PlatformTransactionManager transactionManager, SequentialDatasets datasets,
                                 UtilityStep body) {
        return new JobBuilder(jobName, jobRepository)
                .start(new StepBuilder(jobName.toUpperCase(Locale.ROOT), jobRepository)
                        .tasklet((contribution, chunk) -> {
                            StepExecution step = chunk.getStepContext().getStepExecution();
                            Counts counts;
                            try (Sysout sysprint = datasets.sysout(step, jobName)) {
                                counts = body.run(step, sysprint);
                            } catch (FileStatusException e) {
                                if (e.status() == FileStatus.FILE_NOT_FOUND) {
                                    // A DD naming no dataset is a JCL error on z/OS: the job is flushed, so the
                                    // remaining steps (no COND=EVEN in these jobs) are bypassed like after an abend.
                                    throw new AbendException(0, jobName + ": JCL ERROR, DATA SET NOT FOUND - "
                                            + e.getMessage(), e);
                                }
                                throw new ReturnCodeException(ReturnCode.TERMINAL,
                                        jobName + ": " + e.getMessage());
                            }
                            for (long i = 0; i < counts.read(); i++) {
                                contribution.incrementReadCount();
                            }
                            contribution.incrementWriteCount(counts.written());
                            ReturnCode.set(step, counts.returnCode());
                            return RepeatStatus.FINISHED;
                        }, transactionManager).build())
                .build();
    }

    /** {@code parameters} plus each default whose name is not given. */
    public static JobParameters with(JobParameters parameters, Map<String, String> defaults) {
        JobParametersBuilder builder = new JobParametersBuilder(parameters);
        defaults.forEach((name, value) -> {
            if (parameters.getParameter(name) == null) {
                builder.addString(name, value);
            }
        });
        return builder.toJobParameters();
    }

    static JobStream stream(String name, java.util.function.BiFunction<BatchJobLauncher, JobParameters, JobChain> chain) {
        return new JobStream() {
            @Override
            public String name() {
                return name;
            }

            @Override
            public JobChain chain(BatchJobLauncher launcher, JobParameters parameters) {
                return chain.apply(launcher, parameters);
            }
        };
    }

    static Dataset dataset(JobParameters parameters) {
        String name = parameters.getString(DATASET, "").toUpperCase(Locale.ROOT);
        if (!name.equals(Dataset.TRANSACT.name()) && !name.equals(Dataset.TCATBALF.name())) {
            throw new ReturnCodeException(ReturnCode.TERMINAL, "--DATASET must be TRANSACT or TCATBALF, got '"
                    + name + "'");
        }
        return Dataset.valueOf(name);
    }

    static int keyLength(Dataset dataset) {
        return dataset == Dataset.TRANSACT ? 16 : 17;
    }

    private static String key(FixedWidthRecord record, int keyLength) {
        return HexFormat.of().formatHex(record.bytes(), 0, keyLength);
    }

    private static String source(JobParameters parameters, String dd) {
        return DdParameters.isTable(parameters, dd) ? "table" : DdParameters.path(parameters, dd).toString();
    }

    private static ReturnCodeException severe(Sysout sysprint, String message) {
        sysprint.display(message);
        sysprint.display(FUNCTION_COMPLETED + ReturnCode.SEVERE.code());
        return new ReturnCodeException(ReturnCode.SEVERE, message);
    }
}
