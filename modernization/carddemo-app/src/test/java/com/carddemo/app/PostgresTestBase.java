package com.carddemo.app;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.testcontainers.containers.PostgreSQLContainer;

/** One Testcontainers {@code postgres:16} for every Spring test class (singleton container, removed by Ryuk). */
@SpringBootTest(classes = CardDemoApplication.class)
@ActiveProfiles("test")
public abstract class PostgresTestBase {

    static final PostgreSQLContainer<?> POSTGRES = new PostgreSQLContainer<>("postgres:16");

    static {
        POSTGRES.start();
    }

    @DynamicPropertySource
    static void datasource(DynamicPropertyRegistry registry) {
        registry.add("spring.datasource.url", POSTGRES::getJdbcUrl);
        registry.add("spring.datasource.username", POSTGRES::getUsername);
        registry.add("spring.datasource.password", POSTGRES::getPassword);
    }

    @Autowired
    protected JdbcTemplate jdbc;

    protected void truncateBusinessTables() {
        jdbc.execute("TRUNCATE account.account, card.card_xref, reference.tran_cat_balance, batch.posttran_reject");
    }
}
