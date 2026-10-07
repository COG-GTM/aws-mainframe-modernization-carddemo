package com.carddemo;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.List;
import org.flywaydb.core.Flyway;
import org.junit.jupiter.api.Test;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.boot.testcontainers.service.connection.ServiceConnection;
import org.springframework.context.annotation.Bean;
import org.springframework.core.env.Environment;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.transaction.PlatformTransactionManager;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

/**
 * Boots the whole application against PostgreSQL 16 (Testcontainers): Flyway, JPA, Spring Batch, web,
 * actuator and springdoc are wired, and a trivial job runs on the Flyway-owned job repository.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Testcontainers
class CardDemoApplicationIT {

    @Container
    @ServiceConnection
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:16-alpine");

    @Autowired
    TestRestTemplate http;

    @Autowired
    JdbcTemplate jdbc;

    @Autowired
    Flyway flyway;

    @Autowired
    Environment environment;

    @Autowired
    JobLauncher jobLauncher;

    @Autowired
    Job scaffoldSmokeJob;

    @Test
    void flywayOwnsTheSpringBatchSchema() {
        assertThat(flyway.info().current().getVersion().getVersion()).isEqualTo("6");
        List<String> tables = jdbc.queryForList(
                "select table_name from information_schema.tables"
                        + " where table_name like 'batch\\_job%' or table_name like 'batch\\_step%' order by 1",
                String.class);
        assertThat(tables).containsExactly("batch_job_execution", "batch_job_execution_context",
                "batch_job_execution_params", "batch_job_instance", "batch_step_execution",
                "batch_step_execution_context");
    }

    @Test
    void jobRunsOnThePostgresJobRepository() throws Exception {
        JobExecution execution = jobLauncher.run(scaffoldSmokeJob,
                new JobParametersBuilder().addLong("run", System.nanoTime()).toJobParameters());

        assertThat(execution.getStatus()).isEqualTo(BatchStatus.COMPLETED);
        assertThat(jdbc.queryForObject("select count(*) from batch_job_execution where status = 'COMPLETED'",
                Integer.class)).isPositive();
    }

    @Test
    void healthIsUp() {
        ResponseEntity<String> health = http.getForEntity("/actuator/health", String.class);
        assertThat(health.getStatusCode()).isEqualTo(HttpStatus.OK);
        assertThat(health.getBody()).contains("\"status\":\"UP\"");
        assertThat(health.getBody()).as("test profile shows health details").contains("\"db\":{\"status\":\"UP\"");
    }

    @Test
    void runsWithTheProfilesTheBuildSelects() {
        String selected = System.getProperty("spring.profiles.active", "test");
        assertThat(environment.getActiveProfiles()).containsExactly(selected.split(","));
        assertThat(environment.getActiveProfiles()).contains("test");
    }

    @Test
    void openApiDocumentIsServed() {
        ResponseEntity<String> docs = http.getForEntity("/v3/api-docs", String.class);
        assertThat(docs.getStatusCode()).isEqualTo(HttpStatus.OK);
        assertThat(docs.getBody()).contains("\"openapi\":\"3.");
    }

    @TestConfiguration(proxyBeanMethods = false)
    static class SmokeJob {

        @Bean
        Job scaffoldSmokeJob(JobRepository jobRepository, PlatformTransactionManager transactionManager) {
            return new JobBuilder("scaffoldSmokeJob", jobRepository)
                    .start(new StepBuilder("noop", jobRepository)
                            .tasklet((contribution, chunk) -> RepeatStatus.FINISHED, transactionManager)
                            .build())
                    .build();
        }
    }
}
