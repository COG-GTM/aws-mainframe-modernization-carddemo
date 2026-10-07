package com.carddemo.batch.harness;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.print.PrintProgramsBaselineTest;
import com.carddemo.common.AbendException;
import java.nio.file.Path;
import java.time.Clock;
import java.time.LocalDate;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.DefaultApplicationArguments;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.boot.testcontainers.service.connection.ServiceConnection;
import org.springframework.context.annotation.Bean;
import org.springframework.core.env.Environment;
import org.springframework.transaction.PlatformTransactionManager;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

/**
 * UNT51-11 harness on PostgreSQL 16: CLI requests → job parameters → launch → RC (as the CLI exit code) and
 * {@code batch_run} rows; READACCT end to end from the tables loaded by {@code initial-load}; JCL {@code COND}
 * chaining over a job whose RC is a parameter.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.NONE,
        properties = "carddemo.initial-load.on-startup=false")
@Testcontainers
class BatchHarnessIT {

    @Container
    @ServiceConnection
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:16-alpine");

    @Autowired
    BatchJobLauncher launcher;

    @Autowired
    BatchRunLog runLog;

    @Autowired
    List<CommandLineJobParameters> adapters;

    @Autowired
    Clock clock;

    @Autowired
    Environment environment;

    @TempDir
    Path dir;

    /** {@code rc-test}: ends with the RC in {@code --rc}; {@code --fail=error} fails with it, {@code abend} abends. */
    @TestConfiguration
    static class RcJob {
        @Bean
        Job rcTestJob(JobRepository jobRepository, PlatformTransactionManager transactionManager) {
            return new JobBuilder("rc-test", jobRepository)
                    .start(new StepBuilder("STEP01", jobRepository).tasklet((contribution, chunk) -> {
                        StepExecution step = chunk.getStepContext().getStepExecution();
                        JobParameters p = step.getJobParameters();
                        ReturnCode rc = ReturnCode.of(Integer.parseInt(p.getString("rc", "0")));
                        String fail = p.getString("fail", "");
                        if (fail.equals("abend")) {
                            throw new AbendException(999, "test abend");
                        }
                        if (fail.equals("error")) {
                            throw new ReturnCodeException(rc, "test failure");
                        }
                        contribution.incrementWriteCount(3);
                        ReturnCode.set(step, rc);
                        return RepeatStatus.FINISHED;
                    }, transactionManager).build())
                    .build();
        }
    }

    private int cli(String... args) {
        BatchExitCodes exitCodes = new BatchExitCodes();
        new BatchCommandLineRunner(launcher, exitCodes, runLog, adapters, clock, environment)
                .run(new DefaultApplicationArguments(args));
        return exitCodes.getExitCode();
    }

    private BatchRun lastJobRow(String jobName) {
        List<BatchRun> rows = runLog.findByJobName(jobName).stream().filter(BatchRun::isJobRow).toList();
        assertThat(rows).isNotEmpty();
        return rows.get(rows.size() - 1);
    }

    @Test
    void readacctFromTheLoadedTablesMatchesTheBaselineAndIsRecorded() throws Exception {
        assertThat(cli("--job=initial-load")).isZero();
        assertThat(launcher.jobNames()).contains("readacct", "readcard", "readxref", "readcust", "initial-load");

        assertThat(cli("--job=READACCT", "--run-date=2022-07-06", "--encoding=ASCII",
                "--record-prefix=GNUCOBOL_VARSEQ_0", "--OUTFILE=" + dir.resolve("OUTFILE"),
                "--ARRYFILE=" + dir.resolve("ARRYFILE"), "--VBRCFILE=" + dir.resolve("VBRCFILE"),
                "--SYSOUT=" + dir.resolve("sysout.txt"))).isZero();

        // ACCTDATA record 49: ZIP 'ZEROAPR' in app/data/EBCDIC (loaded) vs 'A000000000' in the baseline's ASCII input
        List<String> expected = PrintProgramsBaselineTest.baselineSysout("READACCT").stream()
                .map(l -> l.startsWith("00000000049") ? l.replace("A000000000", "ZEROAPR").stripTrailing() : l)
                .toList();
        assertThat(PrintProgramsBaselineTest.sysoutLines(dir.resolve("sysout.txt"))).containsExactlyElementsOf(expected);
        assertThat(PrintProgramsBaselineTest.fold(dir.resolve("OUTFILE"), 107))
                .containsExactlyElementsOf(PrintProgramsBaselineTest.baselineFile("READACCT", "OUTFILE"));
        assertThat(PrintProgramsBaselineTest.foldVarseq0(dir.resolve("VBRCFILE")))
                .containsExactlyElementsOf(PrintProgramsBaselineTest.baselineFile("READACCT", "VBRCFILE"));

        BatchRun job = lastJobRow("readacct");
        assertThat(job.status()).isEqualTo("COMPLETED");
        assertThat(job.returnCode()).isEqualTo(ReturnCode.OK);
        assertThat(job.readCount()).isEqualTo(50);
        assertThat(job.writeCount()).isEqualTo(200);
        assertThat(job.runDate()).isEqualTo(LocalDate.of(2022, 7, 6));
        assertThat(job.exitCode()).isEqualTo("COMPLETED");
        assertThat(job.parameters()).contains("OUTFILE=", "encoding=ASCII").doesNotContain("password");
        List<BatchRun> rows = runLog.findByJobExecutionId(job.jobExecutionId());
        assertThat(rows).extracting(BatchRun::stepName).containsExactly(null, "STEP05");
        assertThat(rows.get(1).readCount()).isEqualTo(50);
    }

    @Test
    void failuresEndWithTheirConditionCode() {
        assertThat(cli("--job=readcard", "--CARDFILE=" + dir.resolve("missing"), "--SYSOUT=" + dir.resolve("s")))
                .isEqualTo(16);
        BatchRun abend = lastJobRow("readcard");
        assertThat(abend.status()).isEqualTo("FAILED");
        assertThat(abend.returnCode()).isEqualTo(ReturnCode.TERMINAL);
        assertThat(abend.message()).contains("ERROR OPENING CARDFILE");

        assertThat(cli("--job=no-such-job")).isEqualTo(16);
        BatchRun unknown = lastJobRow("no-such-job");
        assertThat(unknown.status()).isEqualTo("ABANDONED");
        assertThat(unknown.jobExecutionId()).isNull();
        assertThat(cli("--job=readcard", "--run-date=2022-02-30")).isEqualTo(16);
        assertThat(cli("--job=initial-load", "--mode=MERGE")).isEqualTo(16);

        assertThat(cli("--job=rc-test", "--rc=4")).isEqualTo(4);
        BatchRun warning = lastJobRow("rc-test");
        assertThat(warning.status()).isEqualTo("COMPLETED");
        assertThat(warning.returnCode()).isEqualTo(ReturnCode.WARNING);
        assertThat(warning.writeCount()).isEqualTo(3);
        assertThat(cli("--job=rc-test", "--rc=8", "--fail=error")).isEqualTo(8);
        assertThat(cli("--job=rc-test", "--rc=12", "--fail=error")).isEqualTo(12);
        assertThat(lastJobRow("rc-test").status()).isEqualTo("FAILED");
        assertThat(cli("--job=rc-test", "--fail=abend")).isEqualTo(16);
        assertThat(cli()).isZero();
    }

    private static JobParameters rc(int rc, String fail) {
        return new JobParametersBuilder().addString("rc", String.valueOf(rc)).addString("fail", fail)
                .addLong("run.id", System.nanoTime()).toJobParameters();
    }

    @Test
    void jclCondChaining() {
        JobChain.Result result = new JobChain(launcher)
                .step("STEP01", "rc-test", rc(4, ""), null)
                .step("STEP02", "rc-test", rc(0, ""), "(4,LT)")
                .step("STEP03", "rc-test", rc(0, ""), "(4,LE)")
                .step("STEP04", "rc-test", rc(8, "error"), "(0,NE,STEP02)")
                .step("STEP05", "rc-test", rc(0, ""), "(8,EQ,STEP04)")
                .step("STEP06", "rc-test", rc(0, "abend"), null)
                .step("STEP07", "rc-test", rc(0, ""), null)
                .step("STEP08", "rc-test", rc(0, ""), "EVEN")
                .step("STEP09", "rc-test", rc(4, ""), "ONLY")
                .run();
        assertThat(result.steps()).extracting(s -> s.step().stepName() + (s.bypassed() ? ":bypassed"
                        : ":" + s.outcome().returnCode().code()))
                .containsExactly("STEP01:4", "STEP02:0", "STEP03:bypassed", "STEP04:8", "STEP05:bypassed",
                        "STEP06:16", "STEP07:bypassed", "STEP08:0", "STEP09:4");
        assertThat(result.step("STEP04").outcome().status()).isEqualTo(BatchStatus.FAILED);
        assertThat(result.abended()).isTrue();
        assertThat(result.maxReturnCode()).isEqualTo(ReturnCode.TERMINAL);
    }
}
