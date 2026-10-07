package com.carddemo.batch.exchange;

import com.carddemo.account.AccountRecord;
import com.carddemo.account.AccountRepository;
import com.carddemo.batch.BatchOutputFile;
import com.carddemo.batch.DatedOutputFiles;
import com.carddemo.batch.load.LoadMode;
import com.carddemo.batch.load.LoadResult;
import com.carddemo.batch.load.VsamDatasetLoader;
import com.carddemo.card.CardRecord;
import com.carddemo.card.CardRepository;
import com.carddemo.card.CardXrefRecord;
import com.carddemo.card.CardXrefRepository;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.file.RecordFiles;
import com.carddemo.customer.CustomerRecord;
import com.carddemo.customer.CustomerRepository;
import com.carddemo.transaction.TransactionRecord;
import com.carddemo.transaction.TransactionRepository;
import java.nio.file.Path;
import java.time.Clock;
import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Stream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.domain.Sort;
import org.springframework.transaction.PlatformTransactionManager;

/**
 * {@code cbexport} and {@code cbimport}: CBEXPORT/CBIMPORT over PostgreSQL and dated files (ADR-0012).
 *
 * <ul>
 *   <li>{@code cbexport} reads customers, accounts, cross-references, transactions and cards in key order and writes
 *       one {@code EXPORT.DATA} generation of 500-byte {@code CVEXPORT} records. Parameters: {@code encoding}
 *       (default {@code EBCDIC}), {@code run.id}.</li>
 *   <li>{@code cbimport} splits an export file into {@code CUSTDATA.IMPORT}, {@code ACCTDATA.IMPORT},
 *       {@code CARDXREF.IMPORT}, {@code TRANSACT.IMPORT}, {@code CARDDATA.IMPORT} and {@code IMPORT.ERRORS}
 *       generations in the original dataset layouts. Parameters: {@code input-file} (default: generation (0) of
 *       {@code EXPORT.DATA}), {@code encoding}, {@code run.id}, and optional {@code load-mode}
 *       ({@code REPLACE}/{@code UPSERT}) to load the split datasets back into their tables.</li>
 * </ul>
 */
@Configuration(proxyBeanMethods = false)
public class ExchangeJobConfiguration {

    public static final String EXPORT_JOB = "cbexport";
    public static final String IMPORT_JOB = "cbimport";
    public static final String EXPORT_GDG_BASE = "EXPORT.DATA";
    public static final String ERRORS_GDG_BASE = "IMPORT.ERRORS";
    public static final String ENCODING = "encoding";
    public static final String INPUT_FILE = "input-file";
    public static final String LOAD_MODE = "load-mode";

    private static final Logger log = LoggerFactory.getLogger(ExchangeJobConfiguration.class);

    @Bean
    Job cbexportJob(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                    CustomerRepository customers, AccountRepository accounts, CardXrefRepository xrefs,
                    TransactionRepository transactions, CardRepository cards, DatedOutputFiles outputs,
                    Clock clock) {
        return new JobBuilder(EXPORT_JOB, jobRepository).start(new StepBuilder("cbexport-export", jobRepository)
                .tasklet((contribution, chunk) -> {
                    StepExecution step = chunk.getStepContext().getStepExecution();
                    RecordEncoding encoding = encoding(step.getJobParameters());
                    String timestamp = ExportRecordCodec.timestamp(LocalDateTime.now(clock));
                    Map<ExportRecordType, Stream<FixedWidthRecord>> sources = new EnumMap<>(ExportRecordType.class);
                    sources.put(ExportRecordType.CUSTOMER, customers.findAll(Sort.by("custId")).stream()
                            .map(c -> CustomerRecord.MAPPER.toRecord(c.toRecord(), encoding)));
                    sources.put(ExportRecordType.ACCOUNT, accounts.findAll(Sort.by("acctId")).stream()
                            .map(a -> AccountRecord.MAPPER.toRecord(a.toRecord(), encoding)));
                    sources.put(ExportRecordType.CARD_XREF, xrefs.findAll(Sort.by("cardNum")).stream()
                            .map(x -> CardXrefRecord.MAPPER.toRecord(x.toRecord(), encoding)));
                    sources.put(ExportRecordType.TRANSACTION, transactions.findAll(Sort.by("tranId")).stream()
                            .map(t -> TransactionRecord.MAPPER.toRecord(t.toRecord(), encoding)));
                    sources.put(ExportRecordType.CARD, cards.findAll(Sort.by("cardNum")).stream()
                            .map(c -> CardRecord.MAPPER.toRecord(c.toRecord(), encoding)));
                    List<FixedWidthRecord> out = new ArrayList<>();
                    ExecutionContext context = step.getExecutionContext();
                    for (Map.Entry<ExportRecordType, Stream<FixedWidthRecord>> source : sources.entrySet()) {
                        int before = out.size();
                        source.getValue().forEach(r -> out.add(
                                ExportRecordCodec.export(source.getKey(), r, timestamp, out.size() + 1L)));
                        context.putLong(source.getKey() + ".exported", out.size() - before);
                    }
                    BatchOutputFile file = outputs.write(EXPORT_GDG_BASE, step.getJobExecutionId(), out);
                    context.putString("file", file.getFilePath());
                    for (int i = 0; i < out.size(); i++) {
                        contribution.incrementReadCount();
                    }
                    contribution.incrementWriteCount(out.size());
                    log.info("CBEXPORT: {} records ({}) to {}", out.size(), counts(context, ".exported"),
                            file.getFilePath());
                    return RepeatStatus.FINISHED;
                }, transactionManager).build()).build();
    }

    @Bean
    Job cbimportJob(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                    DatedOutputFiles outputs, VsamDatasetLoader loader, Clock clock) {
        return new JobBuilder(IMPORT_JOB, jobRepository).start(new StepBuilder("cbimport-split", jobRepository)
                .tasklet((contribution, chunk) -> {
                    StepExecution step = chunk.getStepContext().getStepExecution();
                    JobParameters params = step.getJobParameters();
                    ExecutionContext job = step.getJobExecution().getExecutionContext();
                    if (!job.containsKey(INPUT_FILE)) {
                        String input = params.getString(INPUT_FILE);
                        job.putString(INPUT_FILE, input != null ? input : outputs.generation(EXPORT_GDG_BASE, 0)
                                .orElseThrow(() -> new IllegalStateException("no " + EXPORT_GDG_BASE
                                        + " generation registered and no input-file given")).toString());
                    }
                    Path input = Path.of(job.getString(INPUT_FILE));
                    RecordEncoding encoding = encoding(params);
                    List<FixedWidthRecord> records =
                            RecordFiles.readFixed("EXPFILE", input, ExportRecordCodec.LAYOUT, encoding);
                    ZonedDateTime now = ZonedDateTime.now(clock);
                    Map<ExportRecordType, List<FixedWidthRecord>> split = new EnumMap<>(ExportRecordType.class);
                    for (ExportRecordType type : ExportRecordType.values()) {
                        split.put(type, new ArrayList<>());
                    }
                    List<FixedWidthRecord> errors = new ArrayList<>();
                    for (FixedWidthRecord record : records) {
                        contribution.incrementReadCount();
                        var type = ExportRecordType.ofCode(ExportRecordCodec.recordType(record));
                        if (type.isEmpty()) {
                            errors.add(ExportRecordCodec.error(record, "Unknown record type encountered", now));
                            continue;
                        }
                        try {
                            split.get(type.get()).add(ExportRecordCodec.importRecord(type.get(), record));
                        } catch (RuntimeException e) {
                            errors.add(ExportRecordCodec.error(record, "Invalid " + type.get().ddname() + " data: "
                                    + e.getMessage(), now));
                        }
                    }
                    ExecutionContext context = step.getExecutionContext();
                    for (ExportRecordType type : ExportRecordType.values()) {
                        BatchOutputFile file =
                                outputs.write(type.importGdgBase(), step.getJobExecutionId(), split.get(type));
                        context.putLong(type + ".imported", split.get(type).size());
                        context.putString(type + ".file", file.getFilePath());
                    }
                    BatchOutputFile errorFile = outputs.write(ERRORS_GDG_BASE, step.getJobExecutionId(), errors);
                    context.putLong("errors", errors.size());
                    context.putString("errors.file", errorFile.getFilePath());
                    contribution.incrementWriteCount(records.size() - errors.size());
                    log.info("CBIMPORT: {} records from {} ({}), errors={}", records.size(), input,
                            counts(context, ".imported"), errors.size());

                    String loadMode = params.getString(LOAD_MODE);
                    if (loadMode != null) {
                        LoadMode mode = LoadMode.valueOf(loadMode.toUpperCase(Locale.ROOT));
                        for (ExportRecordType type : ExportRecordType.values()) {
                            LoadResult result = loader.load(type.dataset(), split.get(type), mode);
                            context.putLong(type + ".loaded", result.loaded());
                            if (!result.rejects().isEmpty()) {
                                throw new IllegalStateException(type.importGdgBase() + ": "
                                        + result.rejects().size() + " rejected record(s), first "
                                        + result.rejects().get(0));
                            }
                        }
                        log.info("CBIMPORT: loaded into PostgreSQL in {} mode ({})", mode,
                                counts(context, ".loaded"));
                    }
                    return RepeatStatus.FINISHED;
                }, transactionManager).build()).build();
    }

    private static RecordEncoding encoding(JobParameters params) {
        return RecordEncoding.of(params.getString(ENCODING, "EBCDIC"));
    }

    private static String counts(ExecutionContext context, String suffix) {
        StringBuilder sb = new StringBuilder();
        for (ExportRecordType type : ExportRecordType.values()) {
            if (context.containsKey(type + suffix)) {
                sb.append(sb.isEmpty() ? "" : ", ").append(type.code()).append('=').append(context.getLong(type + suffix));
            }
        }
        return sb.toString();
    }
}
