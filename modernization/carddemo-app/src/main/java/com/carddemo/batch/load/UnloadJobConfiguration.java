package com.carddemo.batch.load;

import com.carddemo.account.AccountRepository;
import com.carddemo.account.AccountRecord;
import com.carddemo.batch.harness.DdParameters;
import com.carddemo.batch.harness.ReturnCode;
import com.carddemo.batch.harness.ReturnCodeException;
import com.carddemo.card.CardXrefRecord;
import com.carddemo.card.CardXrefRepository;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.file.RecordFiles;
import com.carddemo.transaction.DailyTransactionRecord;
import com.carddemo.transaction.DailyTransactionRepository;
import com.carddemo.transaction.TranCatBalanceRecord;
import com.carddemo.transaction.TranCatBalanceRepository;
import com.carddemo.transaction.TransactionRecord;
import com.carddemo.transaction.TransactionRepository;
import java.nio.file.Path;
import java.util.List;
import java.util.Locale;
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
 * {@code unload}: IDCAMS {@code REPRO} of a table to a sequential file, in key order:
 * {@code --DATASET=ACCTDATA|CARDXREF|TRANSACT|TCATBALF|DALYTRAN --OUTFILE=<path> [--encoding=ASCII|EBCDIC]}.
 * ASCII writes one full-length record per line (the after-image format of {@code docs/validation/baseline}), EBCDIC
 * fixed-length records. Used to compare the tables a job updated with the baseline's KSDS after-images.
 */
@Configuration(proxyBeanMethods = false)
public class UnloadJobConfiguration {

    public static final String UNLOAD = "unload";
    public static final String DATASET = "DATASET";
    public static final String OUTFILE = "OUTFILE";

    @Bean
    Job unloadJob(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                  AccountRepository accounts, CardXrefRepository xrefs, TransactionRepository transactions,
                  TranCatBalanceRepository balances, DailyTransactionRepository dailyTransactions) {
        return new JobBuilder(UNLOAD, jobRepository)
                .start(new StepBuilder("STEP05", jobRepository).tasklet((contribution, chunk) -> {
                    StepExecution step = chunk.getStepContext().getStepExecution();
                    JobParameters parameters = step.getJobParameters();
                    RecordEncoding encoding = DdParameters.encoding(parameters);
                    String dataset = parameters.getString(DATASET, "").toUpperCase(Locale.ROOT);
                    if (DdParameters.isTable(parameters, OUTFILE)) {
                        throw new ReturnCodeException(ReturnCode.TERMINAL, "--OUTFILE=<path> is required");
                    }
                    Path out = DdParameters.path(parameters, OUTFILE);
                    List<FixedWidthRecord> records = switch (dataset) {
                        case "ACCTDATA" -> accounts.findAllByOrderByAcctIdAsc().stream()
                                .map(a -> AccountRecord.MAPPER.toRecord(a.toRecord(), encoding)).toList();
                        case "CARDXREF" -> xrefs.findAllByOrderByCardNumAsc().stream()
                                .map(x -> CardXrefRecord.MAPPER.toRecord(x.toRecord(), encoding)).toList();
                        case "TRANSACT" -> transactions.findAllByOrderByTranIdAsc().stream()
                                .map(t -> TransactionRecord.MAPPER.toRecord(t.toRecord(), encoding)).toList();
                        case "TCATBALF" -> balances.findAllInKeyOrder().stream()
                                .map(b -> TranCatBalanceRecord.MAPPER.toRecord(b.toRecord(), encoding)).toList();
                        case "DALYTRAN" -> dailyTransactions.findAllByOrderByRecordSeqAsc().stream()
                                .map(d -> DailyTransactionRecord.MAPPER.toRecord(d.toRecord(), encoding)).toList();
                        default -> throw new ReturnCodeException(ReturnCode.TERMINAL,
                                "--DATASET must be ACCTDATA, CARDXREF, TRANSACT, TCATBALF or DALYTRAN, got '"
                                        + dataset + "'");
                    };
                    if (encoding == RecordEncoding.EBCDIC) {
                        RecordFiles.writeFixed(dataset, out, records);
                    } else {
                        RecordFiles.writeLines(dataset, out, records, false);
                    }
                    for (int i = 0; i < records.size(); i++) {
                        contribution.incrementReadCount();
                    }
                    contribution.incrementWriteCount(records.size());
                    ReturnCode.set(step, ReturnCode.OK);
                    return RepeatStatus.FINISHED;
                }, transactionManager).build())
                .build();
    }
}
