package com.carddemo.data;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.data.test.PostgresTestcontainersConfiguration;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;
import org.springframework.jdbc.core.JdbcTemplate;

@SpringBootTest
@Import(PostgresTestcontainersConfiguration.class)
class FlywayPostgresIT {

    @Autowired
    JdbcTemplate jdbc;

    @Test
    void runsOnPostgres16() {
        String version = jdbc.queryForObject("SHOW server_version", String.class);

        assertThat(version).startsWith("16.");
    }

    @Test
    void appliesFlywayMigrations() {
        Integer applied = jdbc.queryForObject(
                "SELECT count(*) FROM flyway_schema_history WHERE success AND version = '0'", Integer.class);

        assertThat(applied).isEqualTo(1);
    }
}
