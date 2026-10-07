package com.carddemo.batch.posttran;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.account.Account;
import com.carddemo.account.AccountRecord;
import com.carddemo.account.AccountRepository;
import com.carddemo.batch.harness.BatchJobLauncher;
import com.carddemo.batch.harness.BatchRun;
import com.carddemo.batch.harness.BatchRunLog;
import com.carddemo.batch.harness.JobChain;
import com.carddemo.batch.harness.JobStream;
import com.carddemo.batch.harness.ReturnCode;
import com.carddemo.batch.load.InitialLoadJobConfiguration;
import com.carddemo.batch.load.LoadMode;
import com.carddemo.batch.print.PrintProgramsBaselineTest;
import com.carddemo.card.CardXrefRepository;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.data.CopybookRecordMapper;
import com.carddemo.common.codec.TestData;
import com.carddemo.transaction.DailyTransaction;
import com.carddemo.transaction.DailyTransactionRepository;
import com.carddemo.transaction.TranCatBalance;
import com.carddemo.transaction.TranCatBalanceRecord;
import com.carddemo.transaction.TranCatBalanceRepository;
import com.carddemo.transaction.Transaction;
import com.carddemo.transaction.TransactionRecord;
import com.carddemo.transaction.TransactionRepository;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.Date;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.testcontainers.service.connection.ServiceConnection;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.ActiveProfiles;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

/**
 * POSTTRAN on PostgreSQL 16 through its {@link JobStream} (what {@code --job=posttran} runs), with the {@code golden} clock: every DD a table after
 * {@code initial-load} of the EBCDIC samples. RC, {@code batch_run} rows, the DALYREJS generation and its
 * {@code batch_output_file} row, and the posted tables against {@code docs/validation/baseline/POSTTRAN} (ACCTDATA
 * record 49's ZIP is the documented EBCDIC-vs-ASCII sample difference; FILLER is not persisted, ADR-0011). Then the
 * unit of work: a duplicate TRANFILE key abends the step after the TCATBALF and ACCTFILE updates of that record, which
 * roll back while the 300 earlier postings stay committed, and no DALYREJS generation is catalogued.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.NONE, properties = {
        "carddemo.initial-load.on-startup=false", "carddemo.batch.output-dir=target/posttran-it-output"})
@ActiveProfiles("golden")
@Testcontainers
class PosttranJobIT {

    @Container
    @ServiceConnection
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:16-alpine");

    @Autowired
    BatchJobLauncher launcher;
    @Autowired
    BatchRunLog runLog;
    @Autowired
    List<JobStream> streams;
    @Autowired
    AccountRepository accounts;
    @Autowired
    TranCatBalanceRepository balances;
    @Autowired
    TransactionRepository transactions;
    @Autowired
    DailyTransactionRepository dailyTransactions;
    @Autowired
    CardXrefRepository xrefs;
    @Autowired
    JdbcTemplate jdbc;

    @TempDir
    Path dir;

    private static JobParametersBuilder parameters() {
        return new JobParametersBuilder().addLocalDate("run-date", LocalDate.of(2022, 7, 6))
                .addLong("run.id", System.nanoTime());
    }

    private void initialLoad() {
        JobParameters load = new JobParametersBuilder(InitialLoadJobConfiguration.parameters(
                TestData.resolve("app/data/EBCDIC"), LoadMode.REPLACE)).addLong("run.id", System.nanoTime())
                .toJobParameters();
        assertThat(launcher.run("initial-load", load).returnCode())
                .isEqualTo(ReturnCode.OK);
    }

    /** {@code --job=posttran} with every DD a table, as the batch CLI resolves it (stream → {@link JobChain}). */
    private JobChain.Result posttran() {
        JobParameters parameters = parameters().addString("encoding", "ASCII")
                .addString("STEP10.SYSOUT", dir.resolve("cbtrn01c.txt").toString())
                .addString("STEP15.SYSOUT", dir.resolve("cbtrn02c.txt").toString()).toJobParameters();
        JobStream stream = streams.stream().filter(s -> s.name().equals(PosttranJobConfiguration.POSTTRAN))
                .findFirst().orElseThrow();
        return stream.chain(launcher, parameters).run();
    }

    private BatchRun lastJobRow(String jobName) {
        List<BatchRun> rows = runLog.findByJobName(jobName).stream().filter(BatchRun::isJobRow).toList();
        assertThat(rows).isNotEmpty();
        return rows.get(rows.size() - 1);
    }

    private static <D extends Record> List<D> baseline(String dataset, CopybookRecordMapper<D> mapper)
            throws IOException {
        return PrintProgramsBaselineTest.baselineFile("POSTTRAN", dataset + ".ksds").stream()
                .map(l -> mapper.fromRecord(FixedWidthRecord.fromLine(mapper.layout(), l, RecordEncoding.ASCII)))
                .toList();
    }

    private void assertPostedTablesMatchTheBaseline() throws IOException {
        assertThat(transactions.findAllByOrderByTranIdAsc().stream().map(Transaction::toRecord).toList())
                .containsExactlyElementsOf(baseline("TRANSACT", TransactionRecord.MAPPER));
        assertThat(balances.findAllInKeyOrder().stream().map(TranCatBalance::toRecord).toList())
                .containsExactlyElementsOf(baseline("TCATBALF", TranCatBalanceRecord.MAPPER));

        List<AccountRecord> actual = accounts.findAllByOrderByAcctIdAsc().stream().map(Account::toRecord).toList();
        List<AccountRecord> expected = new ArrayList<>(baseline("ACCTDATA", AccountRecord.MAPPER));
        AccountRecord rec49 = expected.get(48);
        assertThat(rec49.addrZip().trim()).isEqualTo("A000000000");
        assertThat(actual.get(48).addrZip().trim()).isEqualTo("ZEROAPR");
        expected.set(48, new AccountRecord(rec49.acctId(), rec49.activeStatus(), rec49.currBal(),
                rec49.creditLimit(), rec49.cashCreditLimit(), rec49.openDate(), rec49.expirationDate(),
                rec49.reissueDate(), rec49.currCycCredit(), rec49.currCycDebit(), actual.get(48).addrZip(),
                rec49.groupId()));
        assertThat(actual).containsExactlyElementsOf(expected);
    }

    private long generations() {
        return jdbc.queryForObject("select count(*) from batch_output_file where gdg_base = 'DALYREJS'", Long.class);
    }

    @Test
    void posttranFromTheLoadedTablesMatchesTheBaseline() throws IOException {
        initialLoad();
        JobChain.Result result = posttran();
        assertThat(result.maxReturnCode()).isEqualTo(ReturnCode.WARNING);
        assertThat(result.abended()).isFalse();
        assertThat(result.step(PosttranJobConfiguration.STEP15).bypassed()).isFalse();

        assertThat(PrintProgramsBaselineTest.sysoutLines(dir.resolve("cbtrn01c.txt")))
                .containsExactlyElementsOf(PrintProgramsBaselineTest.baselineSysout("CBTRN01C"));
        assertThat(PrintProgramsBaselineTest.sysoutLines(dir.resolve("cbtrn02c.txt")))
                .containsExactlyElementsOf(PrintProgramsBaselineTest.baselineSysout("POSTTRAN").stream()
                        .filter(l -> !l.startsWith("--- IDCAMS-EMU ") && !l.startsWith("IDCAMS-EMU ")).toList());

        BatchRun step10 = lastJobRow("cbtrn01c");
        assertThat(step10.returnCode()).isEqualTo(ReturnCode.OK);
        assertThat(step10.readCount()).isEqualTo(300);
        BatchRun step15 = lastJobRow("cbtrn02c");
        assertThat(step15.status()).isEqualTo("COMPLETED");
        assertThat(step15.returnCode()).isEqualTo(ReturnCode.WARNING);
        assertThat(step15.readCount()).isEqualTo(300);
        assertThat(step15.writeCount()).isEqualTo(262);
        assertThat(runLog.findByJobExecutionId(step15.jobExecutionId())).extracting(BatchRun::stepName)
                .containsExactly(null, PosttranJobConfiguration.STEP15);

        Map<String, Object> generation = jdbc.queryForMap("""
                select business_date, record_count, file_path, job_execution_id from batch_output_file
                where gdg_base = 'DALYREJS' order by output_file_id desc limit 1""");
        assertThat(((Date) generation.get("business_date")).toLocalDate()).isEqualTo(LocalDate.of(2022, 7, 6));
        assertThat(((Number) generation.get("record_count")).longValue()).isEqualTo(38);
        assertThat(((Number) generation.get("job_execution_id")).longValue()).isEqualTo(step15.jobExecutionId());
        Path rejects = Path.of((String) generation.get("file_path"));
        assertThat(rejects.getFileName().toString()).isEqualTo("DALYREJS.2022-07-06." + step15.jobExecutionId());
        assertThat(PrintProgramsBaselineTest.fold(rejects, Cbtrn02c.REJECT_LRECL))
                .containsExactlyElementsOf(PrintProgramsBaselineTest.baselineFile("POSTTRAN", "DALYREJS"));

        assertPostedTablesMatchTheBaseline();
    }

    @Test
    void aFailedPostingRollsBackOnlyItsOwnUpdates() throws IOException {
        initialLoad();
        // A posted transaction that would also pass validation against the after-image of its account, so its
        // copy at the end of DALYTRAN gets through TCATBALF and ACCTFILE and fails only on the TRANFILE WRITE.
        Set<String> posted = PrintProgramsBaselineTest.baselineFile("POSTTRAN", "TRANSACT.ksds").stream()
                .map(l -> l.substring(0, 16)).collect(Collectors.toSet());
        Map<Long, AccountRecord> finalAccounts = baseline("ACCTDATA", AccountRecord.MAPPER).stream()
                .collect(Collectors.toMap(AccountRecord::acctId, Function.identity()));
        DailyTransaction original = dailyTransactions.findAllByOrderByRecordSeqAsc().stream()
                .filter(d -> posted.contains(d.toRecord().tranId()))
                .filter(d -> Cbtrn02c.validate(d.toRecord(), finalAccounts.get(
                        xrefs.findById(d.toRecord().cardNum()).orElseThrow().getAcctId())).reason() == 0)
                .findFirst().orElseThrow();
        dailyTransactions.save(DailyTransaction.from(301, original.toRecord()));
        long generationsBefore = generations();

        JobChain.Result result = posttran();
        assertThat(result.maxReturnCode()).isEqualTo(ReturnCode.TERMINAL);
        assertThat(result.abended()).isTrue();

        BatchRun step15 = lastJobRow("cbtrn02c");
        assertThat(step15.status()).isEqualTo("FAILED");
        assertThat(step15.returnCode()).isEqualTo(ReturnCode.TERMINAL);
        assertThat(Files.readString(dir.resolve("cbtrn02c.txt"))).contains("ABENDING PROGRAM")
                .doesNotContain("TRANSACTIONS PROCESSED");
        assertThat(generations()).isEqualTo(generationsBefore);
        assertPostedTablesMatchTheBaseline();
    }
}
