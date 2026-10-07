package com.carddemo.support;

import com.carddemo.batch.load.VsamDatasetLoader;
import com.carddemo.batch.load.VsamDatasetLoader.Dataset;
import com.carddemo.common.codec.RecordEncoding;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.boot.test.autoconfigure.orm.jpa.TestEntityManager;
import org.springframework.boot.testcontainers.service.connection.ServiceConnection;
import org.springframework.context.annotation.Import;
import org.springframework.jdbc.core.JdbcTemplate;
import org.testcontainers.containers.PostgreSQLContainer;

/**
 * Repository slice on PostgreSQL 16 (Testcontainers) with the Flyway schema: JPA, Spring Data repositories and the
 * dataset loader only. One container is shared by every slice class (and the cached Spring context); each test
 * method runs in a transaction that is rolled back.
 */
@DataJpaTest
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@Import(VsamDatasetLoader.class)
public abstract class PostgresRepositoryTest {

    @ServiceConnection
    static final PostgreSQLContainer<?> POSTGRES = new PostgreSQLContainer<>("postgres:16-alpine");

    static {
        POSTGRES.start();
    }

    @Autowired
    protected VsamDatasetLoader loader;

    @Autowired
    protected TestEntityManager entityManager;

    @Autowired
    protected JdbcTemplate jdbc;

    /** Loads the EBCDIC sample of each dataset, then clears the persistence context so reads hit the database. */
    protected void loadSamples(Dataset... datasets) {
        for (Dataset dataset : datasets) {
            loader.load(dataset, Samples.read(dataset, RecordEncoding.EBCDIC));
        }
        flushAndClear();
    }

    protected void flushAndClear() {
        entityManager.flush();
        entityManager.clear();
    }
}
