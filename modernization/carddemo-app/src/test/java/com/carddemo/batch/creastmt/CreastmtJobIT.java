package com.carddemo.batch.creastmt;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.harness.BatchJobLauncher;
import com.carddemo.batch.harness.JobChain;
import com.carddemo.batch.harness.JobStream;
import com.carddemo.batch.harness.ReturnCode;
import com.carddemo.batch.load.InitialLoadJobConfiguration;
import com.carddemo.batch.load.LoadMode;
import com.carddemo.batch.load.ReproJobConfiguration;
import com.carddemo.batch.print.PrintProgramsBaselineTest;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.codec.TestData;
import com.carddemo.transaction.TransactionRecord;
import com.carddemo.transaction.TransactionRepository;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.Date;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
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
 * CREASTMT on PostgreSQL 16 through its job stream, {@code golden} profile, from the baseline's start state
 * ({@code initial-load} + {@code repro} of the COMBTRAN TRANSACT and INTCALC ACCTDATA after-images): TRXFL.SEQ, the
 * TRXFL cluster, STATEMNT.PS and STATEMNT.HTML are dated generations equal to {@code docs/validation/baseline/CREASTMT}.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.NONE, properties = {
        "carddemo.initial-load.on-startup=false", "carddemo.batch.output-dir=target/creastmt-it-output"})
@ActiveProfiles("golden")
@Testcontainers
class CreastmtJobIT {

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

    @BeforeEach
    void baselineInputState() {
        JobParameters load = new JobParametersBuilder(InitialLoadJobConfiguration.parameters(
                TestData.resolve("app/data/EBCDIC"), LoadMode.REPLACE)).addLong("run.id", System.nanoTime())
                .toJobParameters();
        assertThat(launcher.run("initial-load", load).returnCode()).isEqualTo(ReturnCode.OK);
        for (String[] repro : List.of(new String[] {"TRANSACT", "COMBTRAN"}, new String[] {"ACCTDATA", "INTCALC"})) {
            JobParameters parameters = parameters().addString(ReproJobConfiguration.DATASET, repro[0])
                    .addString(ReproJobConfiguration.INFILE, TestData.resolve(
                            "docs/validation/baseline/" + repro[1] + "/" + repro[0] + ".ksds.txt").toString())
                    .toJobParameters();
            assertThat(launcher.run(ReproJobConfiguration.REPRO, parameters).returnCode()).isEqualTo(ReturnCode.OK);
        }
        assertThat(transactions.count()).isEqualTo(312);
    }

    private JobChain.Result run(JobParametersBuilder builder) {
        JobStream stream = streams.stream().filter(s -> s.name().equals(CreastmtJobConfiguration.CREASTMT))
                .findFirst().orElseThrow();
        return stream.chain(launcher, builder.toJobParameters()).run();
    }

    private long generations(String gdg) {
        return jdbc.queryForObject("select count(*) from batch_output_file where gdg_base = ?", Long.class, gdg);
    }

    private List<String> generation(String gdg, int lrecl, long count) throws IOException {
        Map<String, Object> row = jdbc.queryForMap("""
                select business_date, record_count, file_path from batch_output_file
                where gdg_base = ? order by output_file_id desc limit 1""", gdg);
        assertThat(((Date) row.get("business_date")).toLocalDate()).isEqualTo(LocalDate.of(2022, 7, 6));
        assertThat(((Number) row.get("record_count")).longValue()).isEqualTo(count);
        return PrintProgramsBaselineTest.fold(Path.of((String) row.get("file_path")), lrecl);
    }

    private void assertBaselineOutputs() throws IOException {
        assertThat(generation(CreastmtJobConfiguration.TRXFL_SEQ, 350, 312))
                .containsExactlyElementsOf(PrintProgramsBaselineTest.baselineFile("CREASTMT", "TRXFL.SEQ"));
        assertThat(generation(CreastmtJobConfiguration.TRXFL, 350, 312))
                .containsExactlyElementsOf(PrintProgramsBaselineTest.baselineFile("CREASTMT", "TRXFL.SEQ"));
        assertThat(generation(CreastmtJobConfiguration.STATEMNT_PS, Cbstm03a.STMT_LRECL, 1262))
                .containsExactlyElementsOf(PrintProgramsBaselineTest.baselineFile("CREASTMT", "STMTFILE"));
        assertThat(generation(CreastmtJobConfiguration.STATEMNT_HTML, Cbstm03a.HTML_LRECL, 6632))
                .containsExactlyElementsOf(PrintProgramsBaselineTest.baselineFile("CREASTMT", "HTMLFILE"));
    }

    @Test
    void tableModeStatementsMatchTheBaseline() throws IOException {
        Path sysout = dir.resolve("cbstm03a.txt");
        JobChain.Result result = run(parameters().addString("STEP040.SYSOUT", sysout.toString()));
        assertThat(result.maxReturnCode()).isEqualTo(ReturnCode.OK);
        assertThat(result.steps()).extracting(s -> s.step().stepName())
                .containsExactly("STEP010", "STEP020", "STEP040");
        assertBaselineOutputs();
        assertThat(PrintProgramsBaselineTest.sysoutLines(sysout))
                .containsExactly("Running JCL : CREASTMT Step STEP040");
    }

    @Test
    void theStatementQueryReturnsTheSortOrder() throws IOException {
        List<FixedWidthRecord> table = transactions.findAllForStatements().stream()
                .map(t -> TransactionRecord.MAPPER.toRecord(t.toRecord(), RecordEncoding.ASCII)).toList();
        assertThat(CreastmtJobConfiguration.sort(table)).isEqualTo(table);
        assertThat(table).extracting(r -> CreastmtJobConfiguration.outrec(r, RecordEncoding.ASCII).text())
                .containsExactlyElementsOf(PrintProgramsBaselineTest.baselineFile("CREASTMT", "TRXFL.SEQ"));
    }

    @Test
    void fileModeDdsGiveTheSameStatements() throws IOException {
        JobChain.Result result = run(parameters()
                .addString("STEP010.SORTIN",
                        TestData.resolve("docs/validation/baseline/COMBTRAN/TRANSACT.ksds.txt").toString())
                .addString("STEP040.XREFFILE",
                        TestData.resolve("docs/validation/baseline/XREFFILE/CARDXREF.ksds.txt").toString())
                .addString("STEP040.CUSTFILE", CreastmtTestSupport.CUSTDATA.toString())
                .addString("STEP040.ACCTFILE", CreastmtTestSupport.ACCTDATA.toString()));
        assertThat(result.maxReturnCode()).isEqualTo(ReturnCode.OK);
        assertBaselineOutputs();
    }

    @Test
    void anAbendCataloguesNoStatements() throws IOException {
        long ps = generations(CreastmtJobConfiguration.STATEMNT_PS);
        long html = generations(CreastmtJobConfiguration.STATEMNT_HTML);
        Path empty = Files.createFile(dir.resolve("TRXFL.empty"));
        JobChain.Result result = run(parameters().addString("STEP040.TRNXFILE", empty.toString()));
        assertThat(result.abended()).isTrue();
        assertThat(result.maxReturnCode()).isEqualTo(ReturnCode.TERMINAL);
        assertThat(generations(CreastmtJobConfiguration.STATEMNT_PS)).isEqualTo(ps);
        assertThat(generations(CreastmtJobConfiguration.STATEMNT_HTML)).isEqualTo(html);
    }

    @Test
    void aMissingSortInputBypassesTheLaterSteps() {
        long trxfl = generations(CreastmtJobConfiguration.TRXFL);
        JobChain.Result result = run(parameters().addString("STEP010.SORTIN", dir.resolve("missing").toString()));
        assertThat(result.maxReturnCode()).isNotEqualTo(ReturnCode.OK);
        assertThat(result.step("STEP020").bypassed()).isTrue();
        assertThat(result.step("STEP040").bypassed()).isTrue();
        assertThat(generations(CreastmtJobConfiguration.TRXFL)).isEqualTo(trxfl);
    }
}
