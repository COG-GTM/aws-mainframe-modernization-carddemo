package com.carddemo.interest.batch;

import com.carddemo.interest.domain.Account;
import com.carddemo.interest.domain.AccountId;
import com.carddemo.interest.io.codec.AccountCodec;
import com.carddemo.interest.io.codec.TransactionCodec;
import com.carddemo.interest.repository.InMemoryAccountRepository;
import com.carddemo.interest.repository.InMemoryCardXrefRepository;
import com.carddemo.interest.repository.InMemoryDisclosureGroupRepository;
import com.carddemo.mainframe.cobol.Db2TimestampFormatter;
import com.carddemo.interest.rules.DisclosureGroupRateResolver;
import com.carddemo.interest.rules.InterestTransactionFactory;
import com.carddemo.interest.rules.MonthlyInterestCalculator;
import com.carddemo.interest.rules.TransactionIdSequence;
import com.carddemo.interest.service.FinalAccountPolicy;
import com.carddemo.interest.service.InterestAccrualResult;
import com.carddemo.interest.service.InterestAccrualService;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Java replacement for JCL job {@code INTCALC} step {@code STEP15}
 * ({@code app/jcl/INTCALC.jcl:22-41}), which runs {@code CBACT04C} with a ten-character run date
 * as {@code PARM}.
 *
 * <p>The job reads the four EBCDIC input datasets, runs the interest cycle and writes two output
 * dataset images: the rewritten account master ({@code ACCTFILE}) and the generated interest
 * transactions ({@code TRANSACT}, {@code RECFM=F,LRECL=350}).
 */
public final class InterestCalculationJob {

    /** Output dataset name for the generated interest transactions ({@code TRANSACT} DD). */
    public static final String TRANSACT_DATASET = "AWS.M2.CARDDEMO.SYSTRAN.PS";

    private final Clock clock;
    private final FinalAccountPolicy finalAccountPolicy;

    public InterestCalculationJob(Clock clock, FinalAccountPolicy finalAccountPolicy) {
        this.clock = clock;
        this.finalAccountPolicy = finalAccountPolicy;
    }

    /** Runs the cycle over the dataset dumps in {@code inputDirectory}. */
    public InterestAccrualResult run(Path inputDirectory, String runDate) {
        return run(InterestDatasets.fromDirectory(inputDirectory), runDate);
    }

    /** Runs the cycle over already-loaded dataset images. */
    public InterestAccrualResult run(InterestDatasets datasets, String runDate) {
        InMemoryAccountRepository accounts = new InMemoryAccountRepository(datasets.accounts());
        InterestAccrualService service = new InterestAccrualService(
                accounts,
                new InMemoryCardXrefRepository(datasets.cardXrefs()),
                new DisclosureGroupRateResolver(
                        new InMemoryDisclosureGroupRepository(datasets.disclosureGroups())),
                new MonthlyInterestCalculator(),
                new InterestTransactionFactory(new TransactionIdSequence(runDate),
                        new Db2TimestampFormatter(clock)),
                finalAccountPolicy);
        return service.accrue(datasets.transactionCategoryBalances());
    }

    /**
     * Encodes the account master after the run: the file-level effect of the COBOL {@code REWRITE}
     * against the keyed {@code ACCTFILE}.
     *
     * <p>Accounts the cycle posted are produced by patching their changed fields into their own
     * source record; every other record is copied through byte for byte, exactly as a keyed
     * rewrite leaves the records it never touches.
     */
    public static byte[] encodeAccountMaster(byte[] accountImage, List<Account> updatedAccounts) {
        Map<AccountId, Account> updatedById = new LinkedHashMap<>();
        updatedAccounts.forEach(account -> updatedById.put(account.id(), account));

        int recordLength = AccountCodec.recordLength();
        ByteArrayOutputStream out = new ByteArrayOutputStream(accountImage.length);
        for (int offset = 0; offset + recordLength <= accountImage.length; offset += recordLength) {
            byte[] record = Arrays.copyOfRange(accountImage, offset, offset + recordLength);
            Account updated = updatedById.get(AccountCodec.decode(record).id());
            out.writeBytes(updated == null ? record : AccountCodec.patch(record, updated));
        }
        return out.toByteArray();
    }

    /** Encodes the generated interest transactions as an unblocked {@code LRECL=350} dataset. */
    public static byte[] encodeTransactions(InterestAccrualResult result) {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        result.transactions().forEach(transaction -> out.writeBytes(TransactionCodec.encode(transaction)));
        return out.toByteArray();
    }

    /**
     * Command-line entry point.
     *
     * @param args {@code <input-dataset-directory> <output-directory> [run-date]}; the run date
     *             defaults to the {@code PARM='2022071800'} of {@code app/jcl/INTCALC.jcl:22}
     */
    public static void main(String[] args) {
        if (args.length < 2) {
            System.err.println("Usage: InterestCalculationJob <input-dir> <output-dir> [run-date]");
            System.exit(12);
        }
        Path inputDirectory = Path.of(args[0]);
        Path outputDirectory = Path.of(args[1]);
        String runDate = args.length > 2 ? args[2] : "2022071800";

        InterestCalculationJob job = new InterestCalculationJob(Clock.systemDefaultZone(),
                FinalAccountPolicy.MAINFRAME_PARITY);
        InterestDatasets datasets = InterestDatasets.fromDirectory(inputDirectory);
        InterestAccrualResult result = job.run(datasets, runDate);
        try {
            Files.createDirectories(outputDirectory);
            Files.write(outputDirectory.resolve(InterestDatasets.ACCTFILE_DATASET),
                    encodeAccountMaster(datasets.rawAccountImage(), result.updatedAccounts()));
            Files.write(outputDirectory.resolve(TRANSACT_DATASET), encodeTransactions(result));
        } catch (IOException e) {
            throw new UncheckedIOException("Cannot write output datasets to " + outputDirectory, e);
        }
        System.out.printf("CBACT04C sliver: %d category balances, %d interest transactions, "
                        + "%d accounts updated%n",
                result.categoryBalancesProcessed(), result.transactions().size(),
                result.updatedAccounts().size());
    }
}
