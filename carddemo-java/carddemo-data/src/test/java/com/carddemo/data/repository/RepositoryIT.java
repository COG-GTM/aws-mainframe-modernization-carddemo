package com.carddemo.data.repository;

import com.carddemo.data.test.PostgresTestcontainersConfiguration;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.boot.test.autoconfigure.orm.jpa.TestEntityManager;
import org.springframework.context.annotation.Import;

/** Repository tests on the Flyway schema in Testcontainers PostgreSQL 16 (Hibernate validates the mapping). */
@DataJpaTest
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@Import(PostgresTestcontainersConfiguration.class)
abstract class RepositoryIT {

    @Autowired
    TestEntityManager em;

    /** Writes pending changes and clears the persistence context so the next read hits PostgreSQL. */
    void flushAndClear() {
        em.flush();
        em.clear();
    }
}
