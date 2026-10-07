package com.carddemo.batch.scheduler;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.harness.BatchJobLauncher;
import com.carddemo.batch.harness.JobOutcome;
import com.carddemo.batch.harness.ReturnCode;
import com.carddemo.batch.load.InitialLoadJobConfiguration;
import com.carddemo.batch.load.LoadMode;
import com.carddemo.common.codec.TestData;
import java.nio.file.Path;
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
 * {@code nightly-cycle} on PostgreSQL 16 ({@code golden} profile) from the tables {@code initial-load} fills: one
 * launch runs the 11 in-scope jobs in order, {@code batch_run} gets the cycle row (RC = JCL max) and one row per
 * member with its RC and counts plus the child jobs' own rows; a member that ends above RC 4 bypasses its
 * successors but not the independent branch.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.NONE, properties = {
        "carddemo.initial-load.on-startup=false", "carddemo.batch.output-dir=target/nightly-cycle-it-output"})
@ActiveProfiles("golden")
@Testcontainers
class NightlyCycleJobIT {

    @Container
    @ServiceConnection
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:16-alpine");

    @Autowired
    BatchJobLauncher launcher;
    @Autowired
    JdbcTemplate jdbc;

    @TempDir
    Path dir;

    @BeforeEach
    void sampleData() {
        JobParameters load = new JobParametersBuilder(InitialLoadJobConfiguration.parameters(
                TestData.resolve("app/data/EBCDIC"), LoadMode.REPLACE)).addLong("run.id", System.nanoTime())
                .toJobParameters();
        assertThat(launcher.run("initial-load", load).returnCode()).isEqualTo(ReturnCode.OK);
    }

    private JobParametersBuilder cycle() {
        return new JobParametersBuilder().addLocalDate("run-date", LocalDate.of(2022, 7, 6))
                .addLong("run.id", System.nanoTime()).addString("encoding", "ASCII")
                .addString("READACCT.OUTFILE", dir.resolve("OUTFILE").toString())
                .addString("READACCT.ARRYFILE", dir.resolve("ARRYFILE").toString())
                .addString("READACCT.VBRCFILE", dir.resolve("VBRCFILE").toString());
    }

    private List<Map<String, Object>> memberRows(long cycleId) {
        return jdbc.queryForList("""
                select step_name, exit_code, return_code, read_count, write_count from batch_run
                where job_execution_id = ? and step_name is not null order by batch_run_id""", cycleId);
    }

    @Test
    void oneLaunchRunsEveryInScopeJobAndRecordsItInBatchRun() {
        JobOutcome outcome = launcher.run(NightlyCycle.NAME, cycle().toJobParameters());

        assertThat(outcome.returnCode()).isEqualTo(ReturnCode.WARNING);
        assertThat(outcome.abended()).isFalse();
        long cycleId = outcome.execution().getId();
        Map<String, Object> job = jdbc.queryForMap(
                "select status, return_code from batch_run where job_execution_id = ? and step_name is null", cycleId);
        assertThat(job.get("status")).isEqualTo("COMPLETED");
        assertThat(((Number) job.get("return_code")).intValue()).isEqualTo(4);
        List<Map<String, Object>> members = memberRows(cycleId);
        assertThat(members).extracting(r -> r.get("step_name")).containsExactlyElementsOf(
                NightlyCycle.MEMBERS.stream().map(NightlyCycle.Member::name).toList());
        assertThat(members).allSatisfy(r -> assertThat(r.get("exit_code")).isEqualTo("COMPLETED"));
        assertThat(members).extracting(r -> ((Number) r.get("return_code")).intValue())
                .containsExactly(0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0);
        Map<String, Object> posttran = members.get(4);
        assertThat(((Number) posttran.get("read_count")).longValue()).isEqualTo(600);
        assertThat(((Number) posttran.get("write_count")).longValue()).isEqualTo(262);
        assertThat(jdbc.queryForObject("""
                select count(distinct substring(parameters from 'cycle\\.member=([A-Z0-9]+)')) from batch_run
                where step_name is null and job_execution_id > ? and parameters like '%cycle.member=%'""",
                Long.class, cycleId)).isEqualTo(11);
        assertThat(jdbc.queryForObject("select count(*) from transaction", Long.class)).isEqualTo(312);
    }

    @Test
    void aFailedMemberBypassesItsSuccessorsOnly() {
        JobOutcome outcome = launcher.run(NightlyCycle.NAME, cycle()
                .addString("POSTTRAN.DALYTRAN", dir.resolve("missing-dalytran.txt").toString()).toJobParameters());

        assertThat(outcome.returnCode()).isEqualTo(ReturnCode.TERMINAL);
        List<Map<String, Object>> members = memberRows(outcome.execution().getId());
        assertThat(members).extracting(r -> r.get("exit_code")).containsExactly("COMPLETED", "COMPLETED",
                "COMPLETED", "COMPLETED", members.get(4).get("exit_code"), "BYPASSED", "BYPASSED", "BYPASSED",
                "BYPASSED", "BYPASSED", "BYPASSED");
        assertThat(((Number) members.get(4).get("return_code")).intValue()).isEqualTo(16);
        assertThat(jdbc.queryForObject("select count(*) from transaction", Long.class)).isZero();
    }
}
