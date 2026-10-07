package com.carddemo.batch.intcalc;

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
import com.carddemo.batch.load.ReproJobConfiguration;
import com.carddemo.batch.load.VsamDatasetLoader;
import com.carddemo.batch.print.PrintProgramsBaselineTest;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.codec.TestData;
import com.carddemo.common.data.CopybookRecordMapper;
import com.carddemo.transaction.DisclosureGroupId;
import com.carddemo.transaction.DisclosureGroupRepository;
import com.carddemo.transaction.TranCatBalance;
import com.carddemo.transaction.TranCatBalanceRecord;
import com.carddemo.transaction.TranCatBalanceRepository;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.file.Path;
import java.sql.Date;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
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
 * INTCALC on PostgreSQL 16 through its {@link JobStream} ({@code --job=intcalc}), {@code golden} profile, every DD a
 * table. Start state = the baseline's: {@code initial-load} of the EBCDIC samples, then {@code repro} of the POSTTRAN
 * after-images of ACCTDATA and TCATBALF from {@code docs/validation/baseline/POSTTRAN} (no POSTTRAN run). Checks RC,
 * {@code batch_run}, the SYSTRAN generation and its {@code batch_output_file} row, SYSOUT, and the ACCTDATA/TCATBALF
 * tables against {@code docs/validation/baseline/INTCALC}. The EBCDIC DISCGRP record 34 (DEFAULT/07/0001, rate 15.00
 * instead of 0.00) is loaded but never read: no TCATBALF row has type 07. Then the unit of work: an abend after an
 * account break rolls back that account's rewrite and catalogues no SYSTRAN generation.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.NONE, properties = {
        "carddemo.initial-load.on-startup=false", "carddemo.batch.output-dir=target/intcalc-it-output"})
@ActiveProfiles("golden")
@Testcontainers
class IntcalcJobIT {

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
    DisclosureGroupRepository groups;
    @Autowired
    JdbcTemplate jdbc;

    @TempDir
    Path dir;

    private static JobParametersBuilder parameters() {
        return new JobParametersBuilder().addLocalDate("run-date", LocalDate.of(2022, 7, 6))
                .addLong("run.id", System.nanoTime());
    }

    /** {@code initial-load} (EBCDIC) + {@code repro} of the POSTTRAN after-images the baseline INTCALC read. */
    private void baselineInputState() {
        JobParameters load = new JobParametersBuilder(InitialLoadJobConfiguration.parameters(
                TestData.resolve("app/data/EBCDIC"), LoadMode.REPLACE)).addLong("run.id", System.nanoTime())
                .toJobParameters();
        assertThat(launcher.run("initial-load", load).returnCode()).isEqualTo(ReturnCode.OK);
        for (VsamDatasetLoader.Dataset dataset : List.of(VsamDatasetLoader.Dataset.ACCTDATA,
                VsamDatasetLoader.Dataset.TCATBALF)) {
            JobParameters repro = parameters().addString(ReproJobConfiguration.DATASET, dataset.name())
                    .addString(ReproJobConfiguration.INFILE, TestData.resolve(
                            "docs/validation/baseline/POSTTRAN/" + dataset.name() + ".ksds.txt").toString())
                    .addString("encoding", "ASCII").toJobParameters();
            assertThat(launcher.run(ReproJobConfiguration.REPRO, repro).returnCode()).isEqualTo(ReturnCode.OK);
        }
        assertThat(balances.count()).isEqualTo(100);
    }

    private JobChain.Result intcalc(Path sysout) {
        return intcalc(parameters().addString("STEP15.SYSOUT", sysout.toString()));
    }

    private JobChain.Result intcalc(JobParametersBuilder builder) {
        JobParameters parameters = builder.addString("encoding", "ASCII").toJobParameters();
        JobStream stream = streams.stream().filter(s -> s.name().equals(IntcalcJobConfiguration.INTCALC))
                .findFirst().orElseThrow();
        return stream.chain(launcher, parameters).run();
    }

    private BatchRun lastJobRow() {
        List<BatchRun> rows = runLog.findByJobName(IntcalcJobConfiguration.CBACT04C_JOB).stream()
                .filter(BatchRun::isJobRow).toList();
        assertThat(rows).isNotEmpty();
        return rows.get(rows.size() - 1);
    }

    private static <D extends Record> List<D> baseline(String job, String dataset, CopybookRecordMapper<D> mapper)
            throws IOException {
        return PrintProgramsBaselineTest.baselineFile(job, dataset + ".ksds").stream()
                .map(l -> mapper.fromRecord(FixedWidthRecord.fromLine(mapper.layout(), l, RecordEncoding.ASCII)))
                .toList();
    }

    private long generations() {
        return jdbc.queryForObject("select count(*) from batch_output_file where gdg_base = 'SYSTRAN'", Long.class);
    }

    @Test
    void intcalcFromTheBaselineInputStateMatchesTheBaseline() throws IOException {
        baselineInputState();
        assertThat(groups.findById(new DisclosureGroupId("DEFAULT", "07", 1)).orElseThrow().getIntRate())
                .isEqualByComparingTo("15.00");
        assertThat(balances.findAllInKeyOrder()).noneMatch(b -> b.getId().tranTypeCd().equals("07"));

        Path sysout = dir.resolve("cbact04c.txt");
        JobChain.Result result = intcalc(sysout);
        assertThat(result.maxReturnCode()).isEqualTo(ReturnCode.OK);
        assertThat(result.abended()).isFalse();

        // Table rows carry no FILLER (ADR-0011): the DISPLAYed TCATBALF images end in spaces, not the 22 zeros.
        List<String> expectedSysout = new ArrayList<>();
        for (String line : IntcalcBaselineTest.baselineSysout()) {
            expectedSysout.add(line.matches("\\d{11}\\d{2}\\d{4}.{11}0{22}") ? line.substring(0, 28) : line);
        }
        assertThat(PrintProgramsBaselineTest.sysoutLines(sysout)).containsExactlyElementsOf(expectedSysout);

        BatchRun run = lastJobRow();
        assertThat(run.status()).isEqualTo("COMPLETED");
        assertThat(run.returnCode()).isEqualTo(ReturnCode.OK);
        assertThat(run.readCount()).isEqualTo(100);
        assertThat(run.writeCount()).isEqualTo(50);
        assertThat(runLog.findByJobExecutionId(run.jobExecutionId())).extracting(BatchRun::stepName)
                .containsExactly(null, IntcalcJobConfiguration.STEP15);

        Map<String, Object> generation = jdbc.queryForMap("""
                select business_date, record_count, file_path, job_execution_id from batch_output_file
                where gdg_base = 'SYSTRAN' order by output_file_id desc limit 1""");
        assertThat(((Date) generation.get("business_date")).toLocalDate()).isEqualTo(LocalDate.of(2022, 7, 6));
        assertThat(((Number) generation.get("record_count")).longValue()).isEqualTo(50);
        assertThat(((Number) generation.get("job_execution_id")).longValue()).isEqualTo(run.jobExecutionId());
        Path systran = Path.of((String) generation.get("file_path"));
        assertThat(systran.getFileName().toString()).isEqualTo("SYSTRAN.2022-07-06." + run.jobExecutionId());
        assertThat(PrintProgramsBaselineTest.fold(systran, Cbact04c.TRANSACT_LRECL))
                .containsExactlyElementsOf(PrintProgramsBaselineTest.baselineFile("INTCALC", "TRANSACT"));

        assertThat(accounts.findAllByOrderByAcctIdAsc().stream().map(Account::toRecord).toList())
                .containsExactlyElementsOf(baseline("INTCALC", "ACCTDATA", AccountRecord.MAPPER));
        assertThat(balances.findAllInKeyOrder().stream().map(TranCatBalance::toRecord).toList())
                .containsExactlyElementsOf(baseline("POSTTRAN", "TCATBALF", TranCatBalanceRecord.MAPPER));
    }

    @Test
    void anAbendRollsBackTheAccountRewritesAndCataloguesNoGeneration() {
        baselineInputState();
        List<AccountRecord> before = accounts.findAllByOrderByAcctIdAsc().stream().map(Account::toRecord).toList();
        jdbc.update("delete from card_xref where acct_id = 2");
        long generations = generations();

        Path sysout = dir.resolve("abend.txt");
        JobChain.Result result = intcalc(sysout);
        assertThat(result.maxReturnCode()).isEqualTo(ReturnCode.TERMINAL);
        assertThat(result.abended()).isTrue();
        assertThat(lastJobRow().returnCode()).isEqualTo(ReturnCode.TERMINAL);
        assertThat(generations()).isEqualTo(generations);
        // Account 1 was rewritten at the break to account 2; the step transaction rolled it back.
        assertThat(accounts.findAllByOrderByAcctIdAsc().stream().map(Account::toRecord).toList())
                .containsExactlyElementsOf(before);
        assertThat(before.get(0).currBal()).isNotEqualByComparingTo(BigDecimal.ZERO);

        // DISP=(NEW,CATLG,DELETE) for an explicit TRANSACT file too: the abended step leaves no partial file.
        Path transact = dir.resolve("SYSTRAN");
        JobChain.Result explicit = intcalc(parameters().addString("STEP15.SYSOUT", dir.resolve("abend2.txt").toString())
                .addString("STEP15.TRANSACT", transact.toString()));
        assertThat(explicit.maxReturnCode()).isEqualTo(ReturnCode.TERMINAL);
        assertThat(transact).doesNotExist();
    }
}
