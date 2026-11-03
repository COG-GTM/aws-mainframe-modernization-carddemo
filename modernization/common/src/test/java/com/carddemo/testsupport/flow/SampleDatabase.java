package com.carddemo.testsupport.flow;

import java.nio.file.Path;
import java.sql.Connection;
import java.util.LinkedHashMap;
import java.util.Map;
import javax.sql.DataSource;
import org.flywaydb.core.Flyway;

/**
 * The database of the online flow tests: its own Testcontainers PostgreSQL ({@link #URL}, so the
 * service's other tests keep their empty tables), all five service schemas migrated from their modules'
 * Flyway scripts, and {@code app/data} loaded by the data-loader.
 */
public final class SampleDatabase {

    /** {@code spring.datasource.url} of every flow test: a container of its own, shared by the flow tests. */
    public static final String URL = "jdbc:tc:postgresql:16:///carddemo_flow";

    public static final Path DATA_DIR = Path.of("..", "..", "app", "data");

    /** Schema to the module whose {@code db/migration} creates it (as SampleDataReconciliationTest). */
    private static final Map<String, String> SCHEMAS = new LinkedHashMap<>();

    static {
        SCHEMAS.put("auth", "auth-service");
        SCHEMAS.put("customer", "customer-service");
        SCHEMAS.put("account", "account-service");
        SCHEMAS.put("card", "card-service");
        SCHEMAS.put("transaction", "transaction-service");
    }

    /** The data-loader call ({@code new SampleDataLoader(DATA_DIR, encoder).load(connection, true)}). */
    @FunctionalInterface
    public interface Loader {
        void load(Connection connection) throws Exception;
    }

    private SampleDatabase() {
    }

    /** Migrates every service schema (a no-op for those already current) and reloads the sample data. */
    public static void reload(DataSource dataSource, Loader loader) throws Exception {
        SCHEMAS.forEach((schema, module) -> Flyway.configure()
                .dataSource(dataSource)
                .schemas(schema)
                .defaultSchema(schema)
                .createSchemas(true)
                .locations("filesystem:../" + module + "/src/main/resources/db/migration")
                .load()
                .migrate());
        try (Connection connection = dataSource.getConnection()) {
            loader.load(connection);
        }
    }
}
