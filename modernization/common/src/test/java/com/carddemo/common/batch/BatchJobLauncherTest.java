package com.carddemo.common.batch;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.launch.support.TaskExecutorJobLauncher;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.repository.support.JobRepositoryFactoryBean;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabase;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseBuilder;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseType;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class BatchJobLauncherTest {

    @TempDir
    Path out;

    private EmbeddedDatabase database;
    private DataSourceTransactionManager transactionManager;
    private JobRepository jobRepository;
    private BatchJobLauncher launcher;
    private final List<String> ran = new ArrayList<>();

    @BeforeEach
    void setUp() throws Exception {
        database = new EmbeddedDatabaseBuilder().setType(EmbeddedDatabaseType.H2).generateUniqueName(true)
                .addScript("/org/springframework/batch/core/schema-h2.sql").build();
        transactionManager = new DataSourceTransactionManager(database);
        JobRepositoryFactoryBean factory = new JobRepositoryFactoryBean();
        factory.setDataSource(database);
        factory.setTransactionManager(transactionManager);
        factory.afterPropertiesSet();
        jobRepository = factory.getObject();
        TaskExecutorJobLauncher jobLauncher = new TaskExecutorJobLauncher();
        jobLauncher.setJobRepository(jobRepository);
        jobLauncher.afterPropertiesSet();
        BatchJobRegistry registry = new BatchJobRegistry(Map.of(
                "WARNJOB", job("warnJob", step("STEP05", rc(4)), step("STEP10", CondTasklet.of("(0,NE)", rc(0))),
                        step("STEP15", rc(0))),
                "CLEANJOB", job("cleanJob", step("STEP05", rc(0)), step("STEP10", CondTasklet.of("(0,NE)", rc(0)))),
                "ABENDJOB", job("abendJob", step("STEP05", (c, ctx) -> {
                    throw new IllegalStateException("file status 23");
                }), step("STEP10", rc(0))),
                "PARMJOB", job("parmJob", step("STEP05", (c, ctx) -> {
                    var params = c.getStepExecution().getJobParameters();
                    Clock clock = Clock.fixed(Instant.parse("2030-01-01T00:00:00Z"), ZoneOffset.UTC);
                    ran.add(BatchJobParameters.parm(params, clock) + "|" + BatchJobParameters.runDate(params, clock)
                            + "|" + BatchJobParameters.dd(params, "DALYTRAN").map(Path::getFileName).orElse(null));
                    return RepeatStatus.FINISHED;
                }))));
        launcher = new BatchJobLauncher(jobLauncher, registry, out,
                Clock.fixed(Instant.parse("2022-07-18T00:00:00Z"), ZoneOffset.UTC));
    }

    @AfterEach
    void tearDown() {
        database.shutdown();
    }

    @Test
    void condZeroNeBypassesAfterARcFourStepAndTheExitIsTheHighestRc() throws Exception {
        BatchRun run = launcher.launch(BatchArguments.of("WARNJOB", null, null));

        assertThat(run.execution().getStatus()).isEqualTo(BatchStatus.COMPLETED);
        assertThat(ran).containsExactly("STEP05", "STEP15");
        assertThat(run.returnCode()).isEqualTo(4);
        assertThat(run.outputDir()).isEqualTo(out.resolve("WARNJOB/20220718-000000-000"));
        assertThat(Files.readString(run.outputDir().resolve("RETURN-CODE"))).isEqualTo("4\n");
        assertThat(run.summary()).containsEntry("returnCode", 4);
    }

    @Test
    void condZeroNeRunsWhenEveryEarlierStepReturnedZero() throws Exception {
        BatchRun run = launcher.launch(BatchArguments.of("CLEANJOB", null, null));

        assertThat(ran).containsExactly("STEP05", "STEP10");
        assertThat(run.returnCode()).isZero();
    }

    @Test
    void aFailingStepAbendsTheJobAndLaterStepsDoNotRun() throws Exception {
        BatchRun run = launcher.launch(BatchArguments.of("ABENDJOB", null, null));

        assertThat(run.execution().getStatus()).isEqualTo(BatchStatus.FAILED);
        assertThat(ran).isEmpty();
        assertThat(run.returnCode()).isEqualTo(ReturnCodes.ABEND);
        assertThat(Files.readString(run.outputDir().resolve("RETURN-CODE"))).isEqualTo("16\n");
    }

    @Test
    void jobParametersCarryDateParmAndDds() throws Exception {
        launcher.launch(new BatchArguments("PARMJOB", LocalDate.of(2022, 7, 18), null, null,
                Map.of("DALYTRAN", Path.of("dailytran.txt")), null, null));
        launcher.launch(BatchArguments.of("PARMJOB", null, "2022071800"));

        assertThat(ran).containsExactly("2022071800|2022-07-18|dailytran.txt", "2022071800|2030-01-01|null");
    }

    @Test
    void unknownJobIsAJclError() {
        assertThatThrownBy(() -> launcher.launch(BatchArguments.of("NOSUCH", null, null)))
                .isInstanceOf(BatchJobRegistry.UnknownJobException.class)
                .hasMessageContaining("WARNJOB");
    }

    private Tasklet rc(int returnCode) {
        return (contribution, chunkContext) -> {
            ran.add(contribution.getStepExecution().getStepName());
            ReturnCodes.set(contribution, returnCode);
            return RepeatStatus.FINISHED;
        };
    }

    private Step step(String name, Tasklet tasklet) {
        return new StepBuilder(name, jobRepository).tasklet(tasklet, transactionManager).build();
    }

    private Job job(String name, Step first, Step... rest) {
        var builder = new JobBuilder(name, jobRepository).start(first);
        for (Step step : rest) {
            builder = builder.next(step);
        }
        return builder.build();
    }
}
