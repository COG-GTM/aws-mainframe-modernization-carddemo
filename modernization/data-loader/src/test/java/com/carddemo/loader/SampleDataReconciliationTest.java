package com.carddemo.loader;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Map;
import org.flywaydb.core.Flyway;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

/**
 * Loads {@code app/data} into the schemas the five services' Flyway migrations create and checks the
 * reconciliation report: counts equal the source files, the round trip has no field difference,
 * and {@code docs/migration/validation/sample-data-reconciliation.md} is current.
 */
@Testcontainers
class SampleDataReconciliationTest {

    private static final Path DATA_DIR = Path.of("..", "..", "app", "data");
    private static final Path REPORT = Path.of("..", "..", "docs", "migration", "validation",
            "sample-data-reconciliation.md");

    /** Schema to the module whose {@code db/migration} creates it. */
    private static final Map<String, String> SCHEMAS = Map.of(
            "auth", "auth-service",
            "customer", "customer-service",
            "account", "account-service",
            "card", "card-service",
            "transaction", "transaction-service");

    /** Record counts of the source files (the ticket's acceptance numbers). */
    private static final Map<TableMapping, Integer> SOURCE_COUNTS = Map.of(
            TableMapping.CUSTOMERS, 50,
            TableMapping.ACCOUNTS, 50,
            TableMapping.CARDS, 50,
            TableMapping.CARD_XREF, 50,
            TableMapping.TRANSACTION_TYPES, 7,
            TableMapping.TRANSACTION_CATEGORIES, 18,
            TableMapping.DISCLOSURE_GROUPS, 51,
            TableMapping.CATEGORY_BALANCES, 50,
            TableMapping.DAILY_TRANSACTIONS, 300,
            TableMapping.USERS, 10);

    @Container
    private static final PostgreSQLContainer<?> POSTGRES = new PostgreSQLContainer<>("postgres:16");

    private final PasswordEncoder encoder = new BCryptPasswordEncoder();

    @BeforeEach
    void freshSchemas() throws SQLException {
        try (Connection connection = connect(); Statement statement = connection.createStatement()) {
            for (String schema : SCHEMAS.keySet()) {
                statement.execute("DROP SCHEMA IF EXISTS " + schema + " CASCADE");
            }
        }
        SCHEMAS.forEach((schema, module) -> Flyway.configure()
                .dataSource(POSTGRES.getJdbcUrl(), POSTGRES.getUsername(), POSTGRES.getPassword())
                .schemas(schema)
                .defaultSchema(schema)
                .createSchemas(true)
                .locations("filesystem:../" + module + "/src/main/resources/db/migration")
                .load()
                .migrate());
    }

    @Test
    void flywayLeavesEveryTargetTableEmpty() throws SQLException {
        try (Connection connection = connect()) {
            assertThat(SampleDataLoader.missingTables(connection)).isEmpty();
            assertThat(SampleDataLoader.rowCounts(connection).values()).containsOnly(0L);
        }
    }

    @Test
    void refusesToLoadWhenBatchOutputExists() throws Exception {
        try (Connection connection = connect(); Statement statement = connection.createStatement()) {
            statement.execute("INSERT INTO transaction.transaction_rejects (tran_id, reason_code) VALUES ('X', 102)");
            assertThatThrownBy(() -> new SampleDataLoader(DATA_DIR, encoder).load(connection, false))
                    .isInstanceOf(IllegalStateException.class)
                    .hasMessageContaining("transaction.transaction_rejects (1 rows)");
            assertThat(SampleDataLoader.rowCounts(connection).values()).containsOnly(0L);
        }
    }

    @Test
    void loadsEveryDatasetAndRoundTripsWithoutDifferences() throws Exception {
        try (Connection connection = connect()) {
            Map<TableMapping, Integer> inserted = new SampleDataLoader(DATA_DIR, encoder).load(connection, false);
            assertThat(inserted).containsExactlyInAnyOrderEntriesOf(SOURCE_COUNTS);

            Reconciliation reconciliation = Reconciliation.run(connection, DATA_DIR, encoder);
            for (Reconciliation.DatasetResult result : reconciliation.datasets()) {
                assertThat(result.sourceRecords()).as(result.mapping() + " source records")
                        .isEqualTo(SOURCE_COUNTS.get(result.mapping()));
                assertThat(result.rows()).as(result.mapping() + " rows").isEqualTo(result.sourceRecords());
                assertThat(result.differences()).as(result.mapping() + " round-trip differences").isEmpty();
            }
            assertThat(reconciliation.reconciled()).isTrue();
            assertThat(reconciliation.datasets().stream()
                    .filter(r -> r.mapping() == TableMapping.USERS).findFirst().orElseThrow().passwordsVerified())
                    .isEqualTo(10);
            assertThat(reconciliation.ebcdicDifferences()).containsExactly(
                    "ACCTDATA record 49 ACCT-ADDR-ZIP ascii='A000000000' ebcdic='ZEROAPR   '",
                    "DISCGRP record 34 DIS-INT-RATE ascii=0.00 ebcdic=15.00");

            String report = reconciliation.toMarkdown();
            if (Boolean.getBoolean("reconciliation.write")) {
                Files.createDirectories(REPORT.getParent());
                Files.writeString(REPORT, report);
            }
            assertThat(REPORT).as("committed report; regenerate with -Dreconciliation.write=true").exists();
            assertThat(Files.readString(REPORT)).isEqualTo(report);
        }
    }

    @Test
    void refusesToLoadOverExistingRowsUnlessReplacing() throws Exception {
        SampleDataLoader loader = new SampleDataLoader(DATA_DIR, encoder);
        try (Connection connection = connect()) {
            loader.load(connection, false);
            assertThatThrownBy(() -> loader.load(connection, false))
                    .isInstanceOf(IllegalStateException.class)
                    .hasMessageContaining("not empty");
            assertThat(SampleDataLoader.rowCounts(connection)).containsEntry(TableMapping.ACCOUNTS, 50L);

            try (Statement statement = connection.createStatement()) {
                statement.execute("INSERT INTO transaction.transactions (tran_id, type_cd, cat_cd, amount, card_num)"
                        + " VALUES ('0000000000000001', '01', 1, 10.00, '0500024453765740')");
                statement.execute("INSERT INTO transaction.transaction_rejects (tran_id, reason_code)"
                        + " VALUES ('0000000000000002', 102)");
            }
            loader.load(connection, true);
            assertThat(SampleDataLoader.rowCounts(connection)).containsEntry(TableMapping.DAILY_TRANSACTIONS, 300L);
            for (String table : SampleDataLoader.DERIVED_TABLES) {
                try (Statement statement = connection.createStatement();
                     var rs = statement.executeQuery("SELECT count(*) FROM " + table)) {
                    rs.next();
                    assertThat(rs.getLong(1)).as(table + " after --replace").isZero();
                }
            }
            assertThat(Reconciliation.run(connection, DATA_DIR, encoder).reconciled()).isTrue();
        }
    }

    private static Connection connect() throws SQLException {
        return DriverManager.getConnection(POSTGRES.getJdbcUrl(), POSTGRES.getUsername(), POSTGRES.getPassword());
    }
}
