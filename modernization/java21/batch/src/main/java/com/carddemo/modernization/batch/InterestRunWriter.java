package com.carddemo.modernization.batch;

import com.carddemo.modernization.interest.InterestRun;
import com.carddemo.modernization.records.AccountRecord;
import com.carddemo.modernization.records.TransactionRecord;
import java.nio.charset.Charset;
import java.nio.file.Path;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.StepExecutionListener;
import org.springframework.batch.item.Chunk;
import org.springframework.batch.item.ItemWriter;

/** Writes the CBACT04C results: the new TRANSACT generation, the rewritten ACCTFILE and the SYSOUT. */
final class InterestRunWriter implements ItemWriter<InterestRun>, StepExecutionListener {
    private static final Logger log = LoggerFactory.getLogger(InterestRunWriter.class);

    private final CycleProperties properties;
    private final Charset charset;
    private Path runDir;

    InterestRunWriter(CycleProperties properties, Charset charset) {
        this.properties = properties;
        this.charset = charset;
    }

    @Override
    public void beforeStep(StepExecution stepExecution) {
        runDir = RunDirectory.of(properties.runRoot(), stepExecution.getJobParameters());
    }

    @Override
    public void write(Chunk<? extends InterestRun> chunk) {
        CycleProperties.Interest dd = properties.interest();
        for (InterestRun run : chunk) {
            FixedLengthFiles.writeNew(runDir.resolve(dd.transact()), run.transactions().stream()
                    .map(t -> TransactionRecord.CODEC.encode(t, charset)).toList());
            KeyedDataset<AccountRecord> acctfile = KeyedDataset.load(dd.acctfile(), AccountRecord.CODEC, 11, charset);
            for (AccountRecord account : run.accounts()) {
                if (!acctfile.rewrite(account)) {
                    throw new IllegalStateException("ACCTFILE rewrite of missing account " + account.acctId());
                }
            }
            KeyedDataset.flush(acctfile);
            FixedLengthFiles.writeNewText(runDir.resolve(dd.sysout()), run.sysout());
            log.info("CBACT04C interest transactions={} RC=0", run.transactions().size());
        }
    }
}
