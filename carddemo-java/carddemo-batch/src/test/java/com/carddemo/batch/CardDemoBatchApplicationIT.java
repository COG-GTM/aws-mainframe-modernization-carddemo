package com.carddemo.batch;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.data.test.CoreRepositories;
import com.carddemo.data.test.PostgresTestcontainersConfiguration;
import org.junit.jupiter.api.Test;
import org.springframework.batch.core.explore.JobExplorer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Import;
import org.springframework.jdbc.core.JdbcTemplate;

@SpringBootTest
@Import(PostgresTestcontainersConfiguration.class)
class CardDemoBatchApplicationIT {

    @Autowired
    ApplicationContext context;

    @Autowired
    JobExplorer jobExplorer;

    @Autowired
    JdbcTemplate jdbc;

    @Test
    void startsWithJobRepositoryInPostgres() {
        Integer jobTables = jdbc.queryForObject(
                "SELECT count(*) FROM information_schema.tables WHERE table_name = 'batch_job_instance'",
                Integer.class);

        assertThat(jobTables).isEqualTo(1);
        assertThat(jobExplorer.getJobNames()).isEmpty();
    }

    @Test
    void loadsCarddemoDataRepositories() {
        CoreRepositories.assertLoaded(context);
    }
}
