package com.carddemo.app;

import com.carddemo.common.batch.BatchArguments;
import com.carddemo.common.batch.BatchCli;
import com.carddemo.common.batch.ReturnCode;
import org.flywaydb.core.Flyway;
import org.junit.jupiter.api.Test;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest
@ActiveProfiles("test")
@Testcontainers
class CardDemoApplicationSmokeTest {

    @Container
    static final PostgreSQLContainer<?> POSTGRES = new PostgreSQLContainer<>("postgres:16");

    @DynamicPropertySource
    static void datasource(DynamicPropertyRegistry registry) {
        registry.add("spring.datasource.url", POSTGRES::getJdbcUrl);
        registry.add("spring.datasource.username", POSTGRES::getUsername);
        registry.add("spring.datasource.password", POSTGRES::getPassword);
    }

    @Autowired
    JdbcTemplate jdbc;

    @Autowired
    Flyway flyway;

    @Autowired
    JobLauncher jobLauncher;

    @Autowired
    BatchJobLauncher launcher;

    @Test
    void contextStartsOnPostgres16() {
        String version = jdbc.queryForObject("SHOW server_version", String.class);
        assertThat(version).startsWith("16.");
        assertThat(jobLauncher).isNotNull();
    }

    @Test
    void flywayAppliedOnlyTheBaseline() {
        assertThat(flyway.info().current().getVersion().getVersion()).isEqualTo("0");
        List<String> applied = jdbc.queryForList(
                "SELECT version || ':' || success FROM flyway_schema_history ORDER BY installed_rank", String.class);
        assertThat(applied).containsExactly("0:true");
    }

    @Test
    void noBusinessTablesYet() {
        List<String> tables = jdbc.queryForList("""
                SELECT table_schema || '.' || table_name FROM information_schema.tables
                WHERE table_schema NOT IN ('pg_catalog', 'information_schema')""", String.class);
        assertThat(tables).containsExactly("public.flyway_schema_history");
    }

    @Test
    void unknownJobIsAUsageErrorRc12() {
        ByteArrayOutputStream err = new ByteArrayOutputStream();
        int rc = BatchCli.run(new String[] {"--job=POSTTRAN"}, launcher::run,
                new PrintStream(err, true, StandardCharsets.UTF_8));
        assertThat(rc).isEqualTo(ReturnCode.JCL_ERROR);
        assertThat(err.toString(StandardCharsets.UTF_8)).contains("Unknown job POSTTRAN");
        assertThat(BatchArguments.parse("--job=posttran").job()).isEqualTo("POSTTRAN");
    }
}
