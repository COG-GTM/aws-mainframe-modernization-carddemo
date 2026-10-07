package com.carddemo.batch.housekeeping;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.harness.BatchJobLauncher;
import com.carddemo.batch.harness.JobChain;
import com.carddemo.batch.harness.JobStream;
import com.carddemo.batch.harness.ReturnCode;
import com.carddemo.batch.load.InitialLoadJobConfiguration;
import com.carddemo.batch.load.LoadMode;
import com.carddemo.batch.load.ReproJobConfiguration;
import com.carddemo.batch.load.VsamDatasetLoader;
import com.carddemo.batch.print.PrintProgramsBaselineTest;
import com.carddemo.batch.tranrept.Cbtrn03c;
import com.carddemo.batch.tranrept.TranreptJobConfiguration;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.codec.TestData;
import com.carddemo.transaction.Transaction;
import com.carddemo.transaction.TransactionRecord;
import com.carddemo.transaction.TransactionRepository;
import java.io.IOException;
import java.nio.file.Path;
import java.sql.Date;
import java.time.LocalDate;
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
 * TRANBKP, COMBTRAN, TRANREPT and PRTCATBL on PostgreSQL 16 through their job streams, {@code golden} profile, KSDS
 * DDs as tables and every GDG a dated generation, in the baseline order from the baseline's start state
 * ({@code initial-load} + {@code repro} of the POSTTRAN after-images). COMBTRAN reads TRANBKP's TRANSACT.BKUP(0)
 * generation and the baseline SYSTRAN (INTCALC is not run here); TRANREPT and PRTCATBL read what the previous jobs left.
 * Each output is compared with {@code docs/validation/baseline}.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.NONE, properties = {
        "carddemo.initial-load.on-startup=false", "carddemo.batch.output-dir=target/housekeeping-it-output"})
@ActiveProfiles("golden")
@Testcontainers
class HousekeepingJobIT {

    @Container
    @ServiceConnection
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:16-alpine");

    @Autowired
    BatchJobLauncher launcher;
    @Autowired
    List<JobStream> streams;
    @Autowired
    TransactionRepository transactions;
    @Autowired
    JdbcTemplate jdbc;

    @TempDir
    Path dir;

    private static JobParametersBuilder parameters() {
        return new JobParametersBuilder().addLocalDate("run-date", LocalDate.of(2022, 7, 6))
                .addLong("run.id", System.nanoTime()).addString("encoding", "ASCII");
    }

    private void baselineInputState() {
        JobParameters load = new JobParametersBuilder(InitialLoadJobConfiguration.parameters(
                TestData.resolve("app/data/EBCDIC"), LoadMode.REPLACE)).addLong("run.id", System.nanoTime())
                .toJobParameters();
        assertThat(launcher.run("initial-load", load).returnCode()).isEqualTo(ReturnCode.OK);
        for (VsamDatasetLoader.Dataset dataset : List.of(VsamDatasetLoader.Dataset.TRANSACT,
                VsamDatasetLoader.Dataset.TCATBALF)) {
            JobParameters repro = parameters().addString(ReproJobConfiguration.DATASET, dataset.name())
                    .addString(ReproJobConfiguration.INFILE, TestData.resolve(
                            "docs/validation/baseline/POSTTRAN/" + dataset.name() + ".ksds.txt").toString())
                    .toJobParameters();
            assertThat(launcher.run(ReproJobConfiguration.REPRO, repro).returnCode()).isEqualTo(ReturnCode.OK);
        }
        assertThat(transactions.count()).isEqualTo(262);
    }

    private JobChain.Result run(String name, JobParametersBuilder builder) {
        JobStream stream = streams.stream().filter(s -> s.name().equals(name)).findFirst().orElseThrow();
        return stream.chain(launcher, builder.toJobParameters()).run();
    }

    private Map<String, Object> latest(String gdg) {
        return jdbc.queryForMap("""
                select business_date, record_count, file_path from batch_output_file
                where gdg_base = ? order by output_file_id desc limit 1""", gdg);
    }

    private long generations(String gdg) {
        return jdbc.queryForObject("select count(*) from batch_output_file where gdg_base = ?", Long.class, gdg);
    }

    private List<String> generation(String gdg, int lrecl, long count) throws IOException {
        Map<String, Object> row = latest(gdg);
        assertThat(((Date) row.get("business_date")).toLocalDate()).isEqualTo(LocalDate.of(2022, 7, 6));
        assertThat(((Number) row.get("record_count")).longValue()).isEqualTo(count);
        return PrintProgramsBaselineTest.fold(Path.of((String) row.get("file_path")), lrecl);
    }

    private List<String> transactionTable() {
        return transactions.findAllByOrderByTranIdAsc().stream().map(Transaction::toRecord)
                .map(r -> TransactionRecord.MAPPER.toRecord(r, RecordEncoding.ASCII).text()).toList();
    }

    @Test
    void theHousekeepingAndReportJobsMatchTheBaseline() throws IOException {
        baselineInputState();

        JobChain.Result tranbkp = run(HousekeepingJobConfiguration.TRANBKP, parameters());
        assertThat(tranbkp.maxReturnCode()).isEqualTo(ReturnCode.OK);
        assertThat(generation("TRANSACT.BKUP", 350, 262))
                .containsExactlyElementsOf(PrintProgramsBaselineTest.baselineFile("TRANBKP", "TRANSACT.BKUP"));
        assertThat(transactions.count()).isZero();

        JobChain.Result combtran = run(HousekeepingJobConfiguration.COMBTRAN, parameters().addString(
                "STEP05R.SORTIN02", TestData.resolve("docs/validation/baseline/INTCALC/TRANSACT.txt").toString()));
        assertThat(combtran.maxReturnCode()).isEqualTo(ReturnCode.OK);
        assertThat(generation("TRANSACT.COMBINED", 350, 312)).containsExactlyElementsOf(
                PrintProgramsBaselineTest.baselineFile("COMBTRAN", "TRANSACT.COMBINED"));
        assertThat(transactionTable())
                .containsExactlyElementsOf(PrintProgramsBaselineTest.baselineFile("COMBTRAN", "TRANSACT.ksds"));

        Path sysout = dir.resolve("cbtrn03c.txt");
        JobChain.Result tranrept = run(TranreptJobConfiguration.TRANREPT,
                parameters().addString("STEP15.SYSOUT", sysout.toString()));
        assertThat(tranrept.maxReturnCode()).isEqualTo(ReturnCode.OK);
        assertThat(generation("TRANSACT.BKUP", 350, 312))
                .containsExactlyElementsOf(PrintProgramsBaselineTest.baselineFile("TRANREPT", "TRANSACT.BKUP"));
        assertThat(generation("TRANSACT.DALY", 350, 312))
                .containsExactlyElementsOf(PrintProgramsBaselineTest.baselineFile("TRANREPT", "TRANSACT.DALY"));
        List<String> report = PrintProgramsBaselineTest.baselineFile("TRANREPT", "TRANREPT");
        assertThat(generation("TRANREPT", Cbtrn03c.REPORT_LRECL, report.size())).containsExactlyElementsOf(report);
        List<String> baselineSysout = PrintProgramsBaselineTest.baselineSysout("TRANREPT");
        assertThat(PrintProgramsBaselineTest.sysoutLines(sysout)).containsExactlyElementsOf(baselineSysout.subList(
                baselineSysout.indexOf("--- STEP15 EXEC PGM=CBTRN03C") + 1, baselineSysout.size()));

        JobChain.Result prtcatbl = run(HousekeepingJobConfiguration.PRTCATBL, parameters());
        assertThat(prtcatbl.maxReturnCode()).isEqualTo(ReturnCode.OK);
        // Table rows carry no FILLER (ADR-0011): the backup's FILLER is spaces instead of the sample's 22 zeros.
        assertThat(generation("TCATBALF.BKUP", 50, 100)).extracting(l -> l.substring(0, 28))
                .containsExactlyElementsOf(PrintProgramsBaselineTest.baselineFile("PRTCATBL", "TCATBALF.BKUP")
                        .stream().map(l -> l.substring(0, 28)).toList());
        assertThat(generation("TCATBALF.REPT", HousekeepingJobConfiguration.TCATBALF_REPT_LRECL, 100))
                .containsExactlyElementsOf(PrintProgramsBaselineTest.baselineFile("PRTCATBL", "TCATBALF.REPT"));

        // COMBTRAN again: the merged keys are already in TRANSACT, so the REPRO load ends with RC 12 and loads nothing.
        List<String> before = transactionTable();
        JobChain.Result again = run(HousekeepingJobConfiguration.COMBTRAN, parameters().addString(
                "STEP05R.SORTIN02", TestData.resolve("docs/validation/baseline/INTCALC/TRANSACT.txt").toString()));
        assertThat(again.maxReturnCode()).isEqualTo(ReturnCode.SEVERE);
        assertThat(again.abended()).isFalse();
        assertThat(transactionTable()).isEqualTo(before);
    }

    @Test
    void aFailedBackupBypassesTheReportAndCataloguesNoReport() {
        long reports = generations("TRANREPT");
        JobChain.Result result = run(TranreptJobConfiguration.TRANREPT, parameters()
                .addString("STEP05.FILEIN", dir.resolve("does-not-exist").toString()));
        assertThat(result.maxReturnCode()).isEqualTo(ReturnCode.TERMINAL);
        assertThat(generations("TRANREPT")).isEqualTo(reports);
    }

    @Test
    void defineRefusesANonEmptyCluster() {
        jdbc.update("delete from transaction");
        JobParameters repro = parameters().addString(ReproJobConfiguration.DATASET, "TRANSACT")
                .addString(ReproJobConfiguration.INFILE,
                        TestData.resolve("docs/validation/baseline/POSTTRAN/TRANSACT.ksds.txt").toString())
                .toJobParameters();
        assertThat(launcher.run(ReproJobConfiguration.REPRO, repro).returnCode()).isEqualTo(ReturnCode.OK);
        JobParameters define = parameters().addString(HousekeepingJobConfiguration.DATASET, "TRANSACT")
                .toJobParameters();
        assertThat(launcher.run(HousekeepingJobConfiguration.IDCAMS_DEFINE, define).returnCode())
                .isEqualTo(ReturnCode.SEVERE);
        assertThat(transactions.count()).isEqualTo(262);
    }
}
