package com.carddemo.batch;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.data.PostgresTestcontainersConfiguration;
import org.junit.jupiter.api.Test;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;
import org.springframework.jdbc.core.JdbcTemplate;

@SpringBootTest(properties = "spring.batch.job.enabled=false")
@Import(PostgresTestcontainersConfiguration.class)
class CardDemoBatchApplicationIT {

    @Autowired
    JobRepository jobRepository;

    @Autowired
    JdbcTemplate jdbcTemplate;

    @Test
    void jobRepositoryIsBackedByPostgres() {
        assertThat(jobRepository).isNotNull();
        Integer jobInstances = jdbcTemplate.queryForObject("select count(*) from batch_job_instance", Integer.class);
        assertThat(jobInstances).isNotNull();
    }
}
