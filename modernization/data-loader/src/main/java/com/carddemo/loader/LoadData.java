package com.carddemo.loader;

import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;

/**
 * {@code load-data}: loads {@code app/data} into the five service schemas, then reconciles the tables
 * with the source files. Exit status 0 when reconciled, 1 on a difference, 2 on an error.
 *
 * <pre>
 * java -jar data-loader.jar [--data-dir DIR] [--replace] [--report FILE] [--wait-seconds N]
 * java -jar data-loader.jar --export DIR   (unload the tables to fixed-width files, no load)
 * </pre>
 *
 * Connection from {@code CARDDEMO_DB_URL}, {@code CARDDEMO_DB_USER}, {@code CARDDEMO_DB_PASSWORD};
 * {@code CARDDEMO_DATA_DIR} is the default data directory.
 */
public final class LoadData {

    private LoadData() {
    }

    public static void main(String[] args) {
        try {
            System.exit(run(args));
        } catch (Exception ex) {
            System.err.println("load-data failed: " + ex.getMessage());
            System.exit(2);
        }
    }

    static int run(String[] args) throws Exception {
        Path dataDir = Path.of(env("CARDDEMO_DATA_DIR", "../app/data"));
        boolean replace = false;
        Path report = null;
        int waitSeconds = 180;
        Path export = null;
        for (int i = 0; i < args.length; i++) {
            switch (args[i]) {
                case "--data-dir" -> dataDir = Path.of(args[++i]);
                case "--replace" -> replace = true;
                case "--report" -> report = Path.of(args[++i]);
                case "--wait-seconds" -> waitSeconds = Integer.parseInt(args[++i]);
                case "--export" -> export = Path.of(args[++i]);
                default -> throw new IllegalArgumentException("Unknown argument " + args[i]
                        + "; usage: [--data-dir DIR] [--replace] [--report FILE] [--wait-seconds N] | --export DIR");
            }
        }
        if (export != null) {
            return export(export, waitSeconds);
        }
        if (!Files.isDirectory(dataDir.resolve("ASCII")) || !Files.isDirectory(dataDir.resolve("EBCDIC"))) {
            throw new IllegalArgumentException(dataDir + " has no ASCII/ and EBCDIC/ directories (app/data expected)");
        }
        String url = env("CARDDEMO_DB_URL", "jdbc:postgresql://localhost:5432/carddemo");
        String user = required("CARDDEMO_DB_USER");
        String password = required("CARDDEMO_DB_PASSWORD");
        PasswordEncoder encoder = new BCryptPasswordEncoder();
        if (report != null) {
            // Fail before loading, not after the load has committed.
            Path parent = report.toAbsolutePath().getParent();
            Files.createDirectories(parent);
            if (!Files.isWritable(parent) || Files.isDirectory(report)) {
                throw new IllegalArgumentException("Cannot write report " + report);
            }
        }

        try (Connection connection = DriverManager.getConnection(url, user, password)) {
            awaitSchemas(connection, waitSeconds);
            Map<TableMapping, Integer> inserted = new SampleDataLoader(dataDir, encoder).load(connection, replace);
            inserted.forEach((mapping, rows) -> System.out.printf("loaded %-36s %4d rows%n", mapping.table(), rows));

            Reconciliation reconciliation = Reconciliation.run(connection, dataDir, encoder);
            System.out.print(reconciliation.summary());
            if (report != null) {
                Files.writeString(report, reconciliation.toMarkdown());
                System.out.println("report written to " + report);
            }
            System.out.println(reconciliation.reconciled() ? "reconciled" : "NOT reconciled");
            return reconciliation.reconciled() ? 0 : 1;
        }
    }

    private static int export(Path dir, int waitSeconds) throws Exception {
        try (Connection connection = connect()) {
            awaitSchemas(connection, waitSeconds);
            TableExport.export(connection, dir).forEach((mapping, rows) ->
                    System.out.printf("exported %-36s %4d rows to %s%n", mapping.table(), rows,
                            dir.resolve(mapping.dataset().name())));
            return 0;
        }
    }

    private static Connection connect() throws SQLException {
        return DriverManager.getConnection(env("CARDDEMO_DB_URL", "jdbc:postgresql://localhost:5432/carddemo"),
                required("CARDDEMO_DB_USER"), required("CARDDEMO_DB_PASSWORD"));
    }

    /** The services create the tables with Flyway on startup; wait for all of them. */
    private static void awaitSchemas(Connection connection, int waitSeconds) throws SQLException, InterruptedException {
        long deadline = System.nanoTime() + waitSeconds * 1_000_000_000L;
        List<String> missing = SampleDataLoader.missingTables(connection);
        while (!missing.isEmpty()) {
            if (System.nanoTime() > deadline) {
                throw new IllegalStateException("Tables missing after " + waitSeconds + "s: " + missing
                        + ". Start the services first (docker compose up -d) so Flyway creates the schemas.");
            }
            System.out.println("waiting for Flyway to create " + missing);
            Thread.sleep(3000);
            missing = SampleDataLoader.missingTables(connection);
        }
    }

    private static String env(String name, String fallback) {
        String value = System.getenv(name);
        return value == null || value.isBlank() ? fallback : value;
    }

    private static String required(String name) {
        String value = System.getenv(name);
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException(name + " is not set");
        }
        return value;
    }
}
