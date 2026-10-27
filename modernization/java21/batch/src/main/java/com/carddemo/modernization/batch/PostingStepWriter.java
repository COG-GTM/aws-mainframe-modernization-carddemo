package com.carddemo.modernization.batch;

import com.carddemo.modernization.posting.TransactionPoster;
import com.carddemo.modernization.records.AccountRecord;
import com.carddemo.modernization.records.CardXrefRecord;
import com.carddemo.modernization.records.TranCatBalanceRecord;
import com.carddemo.modernization.records.TransactionRecord;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.time.Clock;
import java.util.ArrayList;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.StepExecutionListener;
import org.springframework.batch.item.Chunk;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemStreamException;
import org.springframework.batch.item.ItemStreamWriter;

/**
 * CBTRN02C step: hands each chunk of DALYTRAN records to {@link TransactionPoster} and backs its {@code Store} with
 * the step's datasets. Indexed files are loaded on open and written back at every chunk commit.
 */
final class PostingStepWriter implements ItemStreamWriter<byte[]>, StepExecutionListener, TransactionPoster.Store {
    static final ExitStatus COMPLETED_WITH_REJECTS = new ExitStatus("COMPLETED_WITH_REJECTS");
    private static final Logger log = LoggerFactory.getLogger(PostingStepWriter.class);

    private final CycleProperties properties;
    private final Clock clock;
    private final Charset charset;

    private TransactionPoster poster;
    private Path runDir;
    private List<String> sysout;
    private KeyedDataset<TransactionRecord> tranfile;
    private KeyedDataset<CardXrefRecord> xreffile;
    private KeyedDataset<AccountRecord> acctfile;
    private KeyedDataset<TranCatBalanceRecord> tcatbalf;
    private OutputStream dalyrejs;
    private long posted;
    private long rejected;

    PostingStepWriter(CycleProperties properties, Clock clock, Charset charset) {
        this.properties = properties;
        this.clock = clock;
        this.charset = charset;
    }

    @Override
    public void beforeStep(StepExecution stepExecution) {
        poster = new TransactionPoster(clock, charset);
        runDir = RunDirectory.of(properties.runRoot(), stepExecution.getJobParameters());
        sysout = new ArrayList<>(List.of("START OF EXECUTION OF PROGRAM CBTRN02C"));
        dalyrejs = null;
        posted = 0;
        rejected = 0;
    }

    @Override
    public void open(ExecutionContext context) {
        CycleProperties.Posting dd = properties.posting();
        tranfile = KeyedDataset.load(dd.tranfile(), TransactionRecord.CODEC, 16, charset);
        xreffile = KeyedDataset.load(dd.xreffile(), CardXrefRecord.CODEC, 16, charset);
        acctfile = KeyedDataset.load(dd.acctfile(), AccountRecord.CODEC, 11, charset);
        tcatbalf = KeyedDataset.load(dd.tcatbalf(), TranCatBalanceRecord.CODEC, 17, charset);
    }

    @Override
    public void write(Chunk<? extends byte[]> chunk) {
        TransactionPoster.Result result = poster.post(List.copyOf(chunk.getItems()), this);
        posted += result.posted();
        rejected += result.rejected();
    }

    @Override
    public void update(ExecutionContext context) {
        openOutputs();
        try {
            dalyrejs.flush();
        } catch (IOException e) {
            throw new ItemStreamException("cannot write DALYREJS", e);
        }
        tranfile.flushIfDirty();
        acctfile.flushIfDirty();
        tcatbalf.flushIfDirty();
        context.putLong("posted", posted);
        context.putLong("rejected", rejected);
    }

    @Override
    public void close() {
        try {
            if (dalyrejs != null) {
                dalyrejs.close();
            }
        } catch (IOException e) {
            throw new ItemStreamException("cannot close DALYREJS", e);
        }
    }

    @Override
    public ExitStatus afterStep(StepExecution stepExecution) {
        boolean completed = stepExecution.getExitStatus().getExitCode().equals(ExitStatus.COMPLETED.getExitCode());
        if (completed) {
            sysout.add(String.format("TRANSACTIONS PROCESSED :%09d", (posted + rejected) % 1_000_000_000L));
            sysout.add(String.format("TRANSACTIONS REJECTED  :%09d", rejected % 1_000_000_000L));
            sysout.add("END OF EXECUTION OF PROGRAM CBTRN02C");
        }
        ExitStatus status = completed && rejected > 0 ? COMPLETED_WITH_REJECTS : null;
        try {
            FixedLengthFiles.writeNewText(runDir.resolve(properties.posting().sysout()), sysout);
        } catch (UncheckedIOException e) {
            log.error("CBTRN02C SYSOUT not written", e);
            status = ExitStatus.FAILED;
        }
        log.info("CBTRN02C posted={} rejected={} RC={}", posted, rejected, rejected > 0 ? 4 : 0);
        return status;
    }

    /** POST-01: TRANFILE and DALYREJS are opened for OUTPUT once per run, however many chunks there are. */
    @Override
    public void openOutputs() {
        if (dalyrejs != null) {
            return;
        }
        tranfile.clear();
        try {
            dalyrejs = new BufferedOutputStream(Files.newOutputStream(runDir.resolve(properties.posting().dalyrejs()),
                    StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE));
        } catch (IOException e) {
            throw new UncheckedIOException("cannot create DALYREJS", e);
        }
    }

    @Override
    public CardXrefRecord findCard(String cardNumber) {
        return xreffile.find(cardNumber);
    }

    @Override
    public AccountRecord findAccount(String accountId) {
        return acctfile.find(accountId);
    }

    @Override
    public TranCatBalanceRecord findCategory(String accountId, String type, String category) {
        TranCatBalanceRecord record = tcatbalf.find(accountId, type, category);
        if (record == null) {
            sysout.add("TCATBAL record not found for key : " + accountId + type + category + ".. Creating.");
        }
        return record;
    }

    @Override
    public void saveCategory(TranCatBalanceRecord record, boolean create) {
        if (create) {
            tcatbalf.insert(record);
        } else if (!tcatbalf.rewrite(record)) {
            throw new IllegalStateException("TCATBALF rewrite of a missing key");
        }
    }

    @Override
    public boolean rewriteAccount(AccountRecord record) {
        return acctfile.rewrite(record);
    }

    @Override
    public void writeTransaction(byte[] record) {
        tranfile.insert(record);
    }

    @Override
    public void writeReject(byte[] record) {
        try {
            dalyrejs.write(record);
        } catch (IOException e) {
            throw new UncheckedIOException("cannot write DALYREJS", e);
        }
    }
}
