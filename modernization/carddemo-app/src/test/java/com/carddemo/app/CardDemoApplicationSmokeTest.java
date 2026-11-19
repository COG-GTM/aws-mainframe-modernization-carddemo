package com.carddemo.app;

import com.carddemo.common.batch.ReturnCode;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.jdbc.core.JdbcTemplate;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest
class CardDemoApplicationSmokeTest extends PostgresContainerSupport {

    @Autowired
    JdbcTemplate jdbc;

    @Test
    void contextRunsOnPostgres16WithFlywayBaseline() {
        assertThat(jdbc.queryForObject("SHOW server_version", String.class)).startsWith("16.");
        assertThat(jdbc.queryForList("SELECT version FROM flyway_schema_history WHERE success ORDER BY installed_rank",
                String.class)).startsWith("1");
        assertThat(jdbc.queryForList("SELECT schema_name FROM information_schema.schemata", String.class))
                .contains("account", "card", "transaction", "reference", "batch");
        assertThat(jdbc.queryForList("SELECT lower(table_name) FROM information_schema.tables WHERE table_schema = 'batch'",
                String.class)).contains("batch_job_instance", "batch_job_execution", "batch_step_execution",
                "batch_job_execution_context", "batch_step_execution_context", "batch_job_execution_params");
    }

    @Test
    void cliNoopJobExitsZero() {
        assertThat(cli("--job=NOOP")).isEqualTo(ReturnCode.OK);
    }

    @Test
    void cliUnknownJobAndBadArgumentsExit12() {
        assertThat(cli("--job=NOSUCH")).isEqualTo(ReturnCode.JCL_ERROR);
        assertThat(cli("--job=NOOP", "--dd=BAD")).isEqualTo(ReturnCode.JCL_ERROR);
    }

    @Test
    void cliRelaunchOfCompletedRunIdExits12() {
        assertThat(cli("--job=NOOP", "--run-id=SMOKE-DONE")).isEqualTo(ReturnCode.OK);
        assertThat(cli("--job=NOOP", "--run-id=SMOKE-DONE")).isEqualTo(ReturnCode.JCL_ERROR);
    }

    private static int cli(String... args) {
        String[] all = Stream.concat(Stream.of(args), Stream.of(datasourceArgs())).toArray(String[]::new);
        return CardDemoApplication.launch(all, new PrintStream(new ByteArrayOutputStream(), true, StandardCharsets.UTF_8));
    }

}
