package com.carddemo.report.batch;

import com.carddemo.recordio.codec.FixedWidthRecord;
import com.carddemo.recordio.codec.RecordEncoding;
import com.carddemo.recordio.layout.CardXref;
import com.carddemo.recordio.layout.CardXrefLayout;
import com.carddemo.recordio.layout.Transaction;
import com.carddemo.recordio.layout.TransactionCategory;
import com.carddemo.recordio.layout.TransactionCategoryLayout;
import com.carddemo.recordio.layout.TransactionLayout;
import com.carddemo.recordio.layout.TransactionType;
import com.carddemo.recordio.layout.TransactionTypeLayout;
import com.carddemo.recordio.store.FixedWidthFile;
import com.carddemo.recordio.store.KeyedRecordStore;
import com.carddemo.report.ReportProperties;
import com.carddemo.report.domain.ReportDateRange;
import com.carddemo.report.domain.ReportLines;
import com.carddemo.report.domain.ReportLookups;
import com.carddemo.report.domain.TransactionReportWriter;
import com.carddemo.report.domain.TransactionSelector;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.PlatformTransactionManager;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.util.List;

/**
 * Job {@code dailyTransactionReportJob} = TRANREPT.jcl as two steps:
 * <ol>
 *   <li>{@code selectAndSortTransactions}: STEP05R REPRO (backup) + STEP05R SORT (date include, card order)</li>
 *   <li>{@code printTransactionReport}: STEP10R CBTRN03C</li>
 * </ol>
 * The sorted subset is handed between steps through the job execution context as a file path.
 */
@Configuration
public class ReportJobConfiguration {

    public static final String JOB_NAME = "dailyTransactionReportJob";
    static final String SORTED_KEY = "sortedTransactions";
    private static final Logger LOG = LoggerFactory.getLogger(ReportJobConfiguration.class);

    @Bean
    public Tasklet selectAndSortTasklet(ReportProperties p) {
        return (contribution, chunkContext) -> {
            RecordEncoding enc = RecordEncoding.of(p.encoding());
            List<Transaction> master = FixedWidthFile.readAll(p.transactionMaster(), TransactionLayout.INSTANCE, enc);
            List<Transaction> daily = TransactionSelector.selectAndSort(master,
                    new ReportDateRange(p.sortStartDate(), p.sortEndDate()));
            var sorted = p.report().resolveSibling(p.report().getFileName() + ".TRANSACT.DALY");
            FixedWidthFile.write(sorted, daily.stream().map(t -> TransactionLayout.INSTANCE.encode(t, enc)).toList());
            chunkContext.getStepContext().getStepExecution().getJobExecution().getExecutionContext()
                    .putString(SORTED_KEY, sorted.toString());
            LOG.info("SORT: {} of {} transactions selected", daily.size(), master.size());
            return RepeatStatus.FINISHED;
        };
    }

    @Bean
    public Tasklet printReportTasklet(ReportProperties p) {
        return (contribution, chunkContext) -> {
            RecordEncoding enc = RecordEncoding.of(p.encoding());
            var ctx = chunkContext.getStepContext().getStepExecution().getJobExecution().getExecutionContext();
            var sortedPath = java.nio.file.Path.of(ctx.getString(SORTED_KEY));
            List<Transaction> daily = FixedWidthFile.readAll(sortedPath, TransactionLayout.INSTANCE, enc);

            ReportDateRange range = readDateParm(p, enc);
            LOG.info("Reporting from {} to {}", range.startDate(), range.endDate());
            ReportLookups lookups = new ReportLookups(
                    KeyedRecordStore.load("CARDXREF", p.cardXref(), CardXrefLayout.INSTANCE, enc, CardXref::cardNumber),
                    KeyedRecordStore.load("TRANTYPE", p.transactionTypes(), TransactionTypeLayout.INSTANCE, enc, TransactionType::typeCode),
                    KeyedRecordStore.load("TRANCATG", p.transactionCategories(), TransactionCategoryLayout.INSTANCE, enc, TransactionCategory::key));
            TransactionReportWriter writer = new TransactionReportWriter(range, lookups, p.outOfRangePolicy());
            List<String> lines = writer.write(daily);
            FixedWidthFile.write(p.report(), lines.stream().map(l -> {
                FixedWidthRecord r = FixedWidthRecord.blank(ReportLines.WIDTH, enc);
                r.setText(0, ReportLines.WIDTH, l);
                return r;
            }).toList());
            if (writer.stoppedOnOutOfRangeRecord()) {
                LOG.warn("run ended on an out-of-range record without totals (CBTRN03C NEXT SENTENCE behaviour)");
            }
            return RepeatStatus.FINISHED;
        };
    }

    private static ReportDateRange readDateParm(ReportProperties p, RecordEncoding enc) {
        try {
            byte[] bytes = Files.readAllBytes(p.dateParm());
            List<FixedWidthRecord> records = FixedWidthFile.split(bytes, ReportDateRange.RECORD_LENGTH, enc);
            if (records.isEmpty()) {
                throw new IllegalStateException("DATEPARM is empty: CBTRN03C would end without producing a report");
            }
            return ReportDateRange.parse(records.get(0));
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    @Bean
    public Step selectAndSortTransactionsStep(JobRepository repo, PlatformTransactionManager tm, Tasklet selectAndSortTasklet) {
        return new StepBuilder("selectAndSortTransactions", repo).tasklet(selectAndSortTasklet, tm).build();
    }

    @Bean
    public Step printTransactionReportStep(JobRepository repo, PlatformTransactionManager tm, Tasklet printReportTasklet) {
        return new StepBuilder("printTransactionReport", repo).tasklet(printReportTasklet, tm).build();
    }

    @Bean
    public Job dailyTransactionReportJob(JobRepository repo, Step selectAndSortTransactionsStep, Step printTransactionReportStep) {
        return new JobBuilder(JOB_NAME, repo).start(selectAndSortTransactionsStep).next(printTransactionReportStep).build();
    }
}
