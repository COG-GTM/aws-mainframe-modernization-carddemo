package com.carddemo.app;

import com.carddemo.common.batch.BatchArguments;
import com.carddemo.common.batch.BatchCli;
import com.carddemo.common.batch.ReturnCode;
import org.flywaydb.core.Flyway;
import org.junit.jupiter.api.Test;
import org.springframework.batch.core.explore.JobExplorer;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class CardDemoApplicationSmokeTest extends PostgresTestBase {

    @Autowired
    Flyway flyway;

    @Autowired
    JobLauncher jobLauncher;

    @Autowired
    JobExplorer jobExplorer;

    @Autowired
    BatchJobLauncher launcher;

    @Test
    void contextStartsOnPostgres16() {
        String version = jdbc.queryForObject("SHOW server_version", String.class);
        assertThat(version).startsWith("16.");
        assertThat(jobLauncher).isNotNull();
    }

    @Test
    void flywayAppliedV0ToV6AndNotV7() {
        assertThat(flyway.info().current().getVersion().getVersion()).isEqualTo("6");
        List<String> applied = jdbc.queryForList(
                "SELECT version || ':' || success FROM flyway_schema_history ORDER BY installed_rank", String.class);
        assertThat(applied).containsExactly("0:true", "1:true", "2:true", "3:true", "4:true", "5:true", "6:true");
    }

    @Test
    void tablesArePhase1WithoutTransact() {
        List<String> tables = jdbc.queryForList("""
                SELECT lower(table_schema || '.' || table_name) FROM information_schema.tables
                WHERE table_schema NOT IN ('pg_catalog', 'information_schema')""", String.class);
        assertThat(tables).containsExactlyInAnyOrder(
                "public.flyway_schema_history",
                "account.account", "card.card_xref", "reference.tran_cat_balance", "batch.posttran_reject",
                "batch.batch_job_instance", "batch.batch_job_execution", "batch.batch_job_execution_params",
                "batch.batch_step_execution", "batch.batch_step_execution_context", "batch.batch_job_execution_context");
        assertThat(jdbc.queryForList("SELECT schema_name FROM information_schema.schemata", String.class))
                .contains("account", "card", "customer", "transaction", "reference", "batch");
    }

    @Test
    void jobRepositoryUsesTheBatchSchema() {
        assertThat(jobExplorer.getJobNames()).isEmpty();
    }

    @Test
    void posttranIsNotRegisteredYet() {
        ByteArrayOutputStream err = new ByteArrayOutputStream();
        int rc = BatchCli.run(new String[] {"--job=POSTTRAN"}, launcher::run,
                new PrintStream(err, true, StandardCharsets.UTF_8));
        assertThat(rc).isEqualTo(ReturnCode.JCL_ERROR);
        assertThat(err.toString(StandardCharsets.UTF_8)).contains("Unknown job POSTTRAN; registered: [LOAD]");
        assertThat(BatchArguments.parse("--job=posttran").job()).isEqualTo("POSTTRAN");
    }
}
