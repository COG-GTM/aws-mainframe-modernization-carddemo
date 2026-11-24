package com.carddemo.data.test;

import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.boot.testcontainers.service.connection.ServiceConnection;
import org.springframework.context.annotation.Bean;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.utility.DockerImageName;

/**
 * Testcontainers PostgreSQL shared by the integration tests of every module (published in the
 * {@code carddemo-data} test-jar). Use with {@code @Import(PostgresTestcontainersConfiguration.class)}.
 */
@TestConfiguration(proxyBeanMethods = false)
public class PostgresTestcontainersConfiguration {

    public static final DockerImageName POSTGRES_IMAGE =
            DockerImageName.parse(System.getProperty("carddemo.postgres.image", "postgres:16-alpine"))
                    .asCompatibleSubstituteFor("postgres");

    @Bean
    @ServiceConnection
    PostgreSQLContainer<?> postgresContainer() {
        return new PostgreSQLContainer<>(POSTGRES_IMAGE);
    }
}
