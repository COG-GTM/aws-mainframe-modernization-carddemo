package com.carddemo.loader;

import com.carddemo.common.record.CobolRecord;

import java.io.IOException;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.springframework.security.crypto.password.PasswordEncoder;

/**
 * Loads every sample dataset under {@code app/data} into the tables the services' Flyway migrations
 * created, in {@link TableMapping} order and in one database transaction.
 */
public final class SampleDataLoader {

    private final Path dataDir;
    private final PasswordEncoder passwordEncoder;

    public SampleDataLoader(Path dataDir, PasswordEncoder passwordEncoder) {
        this.dataDir = dataDir;
        this.passwordEncoder = passwordEncoder;
    }

    /**
     * Loads all datasets and returns the rows inserted per table. Refuses to run when a target table
     * already has rows unless {@code replace} is set, which empties the target tables first.
     */
    public Map<TableMapping, Integer> load(Connection connection, boolean replace) throws SQLException, IOException {
        Map<TableMapping, SourceFile> sources = new LinkedHashMap<>();
        for (TableMapping mapping : TableMapping.values()) {
            sources.put(mapping, SourceFile.read(dataDir, mapping.dataset()));
        }
        boolean autoCommit = connection.getAutoCommit();
        connection.setAutoCommit(false);
        try {
            if (replace) {
                truncate(connection);
            } else {
                requireEmpty(connection);
            }
            Map<TableMapping, Integer> inserted = new LinkedHashMap<>();
            for (Map.Entry<TableMapping, SourceFile> entry : sources.entrySet()) {
                inserted.put(entry.getKey(), insert(connection, entry.getKey(), entry.getValue()));
            }
            connection.commit();
            return inserted;
        } catch (SQLException | RuntimeException ex) {
            connection.rollback();
            throw ex;
        } finally {
            connection.setAutoCommit(autoCommit);
        }
    }

    /** Row count per target table. */
    public static Map<TableMapping, Long> rowCounts(Connection connection) throws SQLException {
        Map<TableMapping, Long> counts = new LinkedHashMap<>();
        try (Statement statement = connection.createStatement()) {
            for (TableMapping mapping : TableMapping.values()) {
                try (ResultSet rs = statement.executeQuery("SELECT count(*) FROM " + mapping.table())) {
                    rs.next();
                    counts.put(mapping, rs.getLong(1));
                }
            }
        }
        return counts;
    }

    /** Target tables that do not exist yet (the owning service has not run its migrations). */
    public static List<String> missingTables(Connection connection) throws SQLException {
        List<String> missing = new ArrayList<>();
        try (PreparedStatement statement = connection.prepareStatement("SELECT to_regclass(?)")) {
            for (TableMapping mapping : TableMapping.values()) {
                statement.setString(1, mapping.table());
                try (ResultSet rs = statement.executeQuery()) {
                    rs.next();
                    if (rs.getString(1) == null) {
                        missing.add(mapping.table());
                    }
                }
            }
        }
        return missing;
    }

    private static void requireEmpty(Connection connection) throws SQLException {
        List<String> loaded = rowCounts(connection).entrySet().stream()
                .filter(e -> e.getValue() > 0)
                .map(e -> e.getKey().table() + " (" + e.getValue() + " rows)")
                .toList();
        if (!loaded.isEmpty()) {
            throw new IllegalStateException("Target tables are not empty: " + String.join(", ", loaded)
                    + ". Use --replace to empty them first, or recreate the database (docker compose down -v).");
        }
    }

    private static void truncate(Connection connection) throws SQLException {
        String tables = Arrays.stream(TableMapping.values()).map(TableMapping::table).collect(Collectors.joining(", "));
        try (Statement statement = connection.createStatement()) {
            statement.execute("TRUNCATE " + tables);
        }
    }

    private int insert(Connection connection, TableMapping mapping, SourceFile source) throws SQLException {
        List<Column> columns = mapping.columns();
        try (PreparedStatement statement = connection.prepareStatement(mapping.insertSql())) {
            int recordNumber = 0;
            for (CobolRecord record : source.records()) {
                recordNumber++;
                try {
                    for (int i = 0; i < columns.size(); i++) {
                        Column column = columns.get(i);
                        Object value = FieldValues.toColumn(record, column);
                        if (column.kind() == Column.Kind.PASSWORD) {
                            value = passwordEncoder.encode((String) value);
                        }
                        FieldValues.bind(statement, i + 1, column, value);
                    }
                } catch (IllegalArgumentException ex) {
                    throw new IllegalArgumentException(source.displayName() + " record " + recordNumber + ": "
                            + ex.getMessage(), ex);
                }
                statement.addBatch();
            }
            return Arrays.stream(statement.executeBatch()).sum();
        }
    }
}
