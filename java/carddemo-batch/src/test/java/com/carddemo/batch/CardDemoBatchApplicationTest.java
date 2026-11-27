package com.carddemo.batch;

import com.carddemo.data.testing.PostgresTestcontainersConfiguration;
import org.junit.jupiter.api.Test;
import org.springframework.batch.core.explore.JobExplorer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;
import org.testcontainers.junit.jupiter.Testcontainers;

import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest
@Import(PostgresTestcontainersConfiguration.class)
@Testcontainers(disabledWithoutDocker = true)
class CardDemoBatchApplicationTest {

    @Autowired
    JobExplorer jobExplorer;

    @Test
    void startsWithJobRepositoryOnPostgres() {
        assertThat(jobExplorer.getJobNames()).isEmpty();
    }
}
