package com.carddemo.batch.tranbkp;

import com.carddemo.batch.refdata.TableSpec;
import com.carddemo.batch.storage.ObjectStore;
import com.carddemo.batch.storage.S3Keys;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.UncheckedIOException;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.sql.ResultSetMetaData;
import java.time.LocalDate;
import java.util.zip.GZIPOutputStream;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;

/** Dumps a table in primary-key order to {@code backup/<table>/<businessDate>/<runId>.csv.gz} (RFC 4180). */
@Component
public class TableBackup {

    private final JdbcTemplate jdbc;
    private final ObjectStore store;

    public TableBackup(JdbcTemplate jdbc, ObjectStore store) {
        this.jdbc = jdbc;
        this.store = store;
    }

    public record Result(String key, int rows) {
    }

    public Result backup(TableSpec spec, LocalDate businessDate, String runId) {
        ByteArrayOutputStream bytes = new ByteArrayOutputStream();
        int[] rows = {0};
        try (Writer w = new OutputStreamWriter(new GZIPOutputStream(bytes), StandardCharsets.UTF_8)) {
            jdbc.query("SELECT * FROM " + spec.table() + " ORDER BY " + String.join(", ", spec.primaryKey()),
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
                    });
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
        String key = S3Keys.backup(spec.table(), businessDate, runId);
        store.put(key, bytes.toByteArray(), "application/gzip");
        return new Result(key, rows[0]);
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
