package com.carddemo.batch.tranbkp;

import com.carddemo.batch.refdata.TableSpec;
import com.carddemo.batch.storage.ObjectStore;
import com.carddemo.batch.storage.S3Keys;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.UncheckedIOException;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.ResultSetMetaData;
import java.time.LocalDate;
import java.util.zip.GZIPOutputStream;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;
import org.springframework.transaction.support.TransactionTemplate;

/**
 * Dumps a table in primary-key order to {@code backup/<table>/<businessDate>/<runId>.csv.gz} (RFC 4180),
 * streaming rows through a JDBC cursor into a temporary gzip file so memory stays bounded as the table grows.
 */
@Component
public class TableBackup {

    static final int FETCH_SIZE = 1000;

    private final JdbcTemplate jdbc;
    private final TransactionTemplate tx;
    private final ObjectStore store;

    public TableBackup(JdbcTemplate jdbc, TransactionTemplate tx, ObjectStore store) {
        this.jdbc = new JdbcTemplate(jdbc.getDataSource());
        this.jdbc.setFetchSize(FETCH_SIZE);
        this.tx = tx;
        this.store = store;
    }

    public record Result(String key, int rows) {
    }

    public Result backup(TableSpec spec, LocalDate businessDate, String runId) {
        Path file;
        try {
            file = Files.createTempFile("carddemo-backup-", ".csv.gz");
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
        try {
            int rows = dump(spec, file);
            String key = S3Keys.backup(spec.table(), businessDate, runId);
            store.put(key, file, "application/gzip");
            return new Result(key, rows);
        } finally {
            try {
                Files.deleteIfExists(file);
            } catch (IOException ignored) {
                // temporary file in the container's ephemeral storage
            }
        }
    }

    /** Cursor-based read (PostgreSQL only honours the fetch size inside a transaction). */
    private int dump(TableSpec spec, Path file) {
        int[] rows = {0};
        try (Writer w = new BufferedWriter(new OutputStreamWriter(
                new GZIPOutputStream(Files.newOutputStream(file)), StandardCharsets.UTF_8))) {
            tx.executeWithoutResult(status -> jdbc.query("SELECT * FROM " + spec.table() + " ORDER BY " + String.join(", ", spec.primaryKey()),
                    rs -> {
                        ResultSetMetaData md = rs.getMetaData();
                        try {
                            if (rows[0] == 0) {
                                for (int i = 1; i <= md.getColumnCount(); i++) {
                                    w.write((i > 1 ? "," : "") + md.getColumnName(i));
                                }
                                w.write("\n");
                            }
                            for (int i = 1; i <= md.getColumnCount(); i++) {
                                w.write((i > 1 ? "," : "") + csv(rs.getString(i)));
                            }
                            w.write("\n");
                        } catch (IOException e) {
                            throw new UncheckedIOException(e);
                        }
                        rows[0]++;
                    }));
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
        return rows[0];
    }

    static String csv(String v) {
        if (v == null) {
            return "";
        }
        if (v.contains(",") || v.contains("\"") || v.contains("\n") || v.contains("\r")
                || !v.equals(v.trim())) {
            return "\"" + v.replace("\"", "\"\"") + "\"";
        }
        return v;
    }
}
