package com.carddemo.posting.batch;

import com.carddemo.mainframe.cobol.Db2TimestampFormatter;
import com.carddemo.mainframe.io.FixedLengthRecordReader;
import com.carddemo.posting.domain.DailyTransaction;
import com.carddemo.posting.io.codec.DailyTransactionCodec;
import com.carddemo.posting.io.codec.PostedTransactionCodec;
import com.carddemo.posting.io.codec.RejectedTransactionCodec;
import com.carddemo.posting.repository.InMemoryAccountRepository;
import com.carddemo.posting.repository.InMemoryCardXrefRepository;
import com.carddemo.posting.repository.InMemoryTransactionCategoryBalanceRepository;
import com.carddemo.posting.rules.PostedTransactionFactory;
import com.carddemo.posting.rules.TransactionValidator;
import com.carddemo.posting.service.DailyTransactionPostingService;
import com.carddemo.posting.service.PostingResult;

import java.io.ByteArrayOutputStream;
import java.nio.file.Path;
import java.time.Clock;
import java.util.List;

/**
 * The batch entry point for daily transaction posting: step {@code STEP15} of job
 * {@code POSTTRAN} ({@code app/jcl/POSTTRAN.jcl:26-43}), which runs {@code CBTRN02C}.
 *
 * <p>This class is the only place that knows the run is a batch job over flat datasets. It
 * assembles the repositories, hands decoded transactions to
 * {@link DailyTransactionPostingService} and encodes the outputs; all business decisions live in
 * the service and the rule classes.
 */
public final class DailyTransactionPostingJob {

    private final Clock clock;

    public DailyTransactionPostingJob(Clock clock) {
        this.clock = clock;
    }

    /** Runs the step against dataset images already in memory. */
    public PostingRunOutput run(PostingDatasets datasets) {
        InMemoryCardXrefRepository cardXrefs = new InMemoryCardXrefRepository(
                datasets.cardXrefImage(), PostingDatasets.CARDXREF_DATASET);
        InMemoryAccountRepository accounts = new InMemoryAccountRepository(
                datasets.accountImage(), PostingDatasets.ACCTFILE_DATASET);
        InMemoryTransactionCategoryBalanceRepository categoryBalances =
                new InMemoryTransactionCategoryBalanceRepository(
                        datasets.categoryBalanceImage(), PostingDatasets.TCATBALF_DATASET);

        DailyTransactionPostingService service = new DailyTransactionPostingService(
                new TransactionValidator(cardXrefs, accounts),
                new PostedTransactionFactory(new Db2TimestampFormatter(clock)),
                accounts,
                categoryBalances);

        PostingResult result = service.post(readDailyTransactions(datasets.dailyTransactionImage()));

        return new PostingRunOutput(result,
                concat(result.posted().stream().map(PostedTransactionCodec::encode).toList()),
                concat(result.rejected().stream().map(RejectedTransactionCodec::encode).toList()),
                accounts.datasetImage(),
                categoryBalances.datasetImage(),
                categoryBalances.createdCount());
    }

    /**
     * Reads the whole daily transaction file up front.
     *
     * <p>The COBOL reads it a record at a time ({@code app/cbl/CBTRN02C.cbl:345-369}) because it
     * has nowhere to put 300 records; the outcome is identical and the loop reads better here.
     * If the file ever outgrows memory this is the single place to make it a stream again.
     */
    private static List<DailyTransaction> readDailyTransactions(byte[] image) {
        return FixedLengthRecordReader
                .split(image, DailyTransactionCodec.recordLength(),
                        PostingDatasets.DALYTRAN_DATASET)
                .stream()
                .map(DailyTransactionCodec::decode)
                .toList();
    }

    private static byte[] concat(List<byte[]> records) {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        records.forEach(out::writeBytes);
        return out.toByteArray();
    }

    /**
     * Runs the step over a directory of unloaded datasets and exits with the COBOL return code
     * ({@code app/cbl/CBTRN02C.cbl:226-232}).
     */
    public static void main(String[] args) {
        if (args.length != 1) {
            System.err.println("usage: DailyTransactionPostingJob <ebcdic-dataset-directory>");
            System.exit(12);
        }
        PostingRunOutput output = new DailyTransactionPostingJob(Clock.systemDefaultZone())
                .run(PostingDatasets.loadFrom(Path.of(args[0])));
        PostingResult result = output.result();
        System.out.printf("TRANSACTIONS PROCESSED :%09d%n", result.transactionsRead());
        System.out.printf("TRANSACTIONS REJECTED  :%09d%n", result.rejected().size());
        System.exit(result.returnCode());
    }
}
