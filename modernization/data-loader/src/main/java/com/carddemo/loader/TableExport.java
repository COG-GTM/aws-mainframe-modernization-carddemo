package com.carddemo.loader;

import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.CodePage;
import com.carddemo.common.record.RecordFormat;
import com.carddemo.common.record.RecordWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Unloads the mapped tables back to fixed-width files (no record terminator, ASCII), one file per
 * dataset named after it ({@code ACCTDATA}, {@code TCATBALF}, ...), in record-key byte order (text keys
 * {@code COLLATE "C"}, as a KSDS sorts them): the VSAM
 * after-image of each table, written with {@link RecordWriter}. Fields without a column (FILLER) are
 * blank. {@code auth.users} is skipped: it holds password hashes, not the source passwords.
 */
final class TableExport {

    private TableExport() {
    }

    static Map<TableMapping, Integer> export(Connection connection, Path dir) throws SQLException, IOException {
        Files.createDirectories(dir);
        Map<TableMapping, Integer> written = new LinkedHashMap<>();
        for (TableMapping mapping : TableMapping.values()) {
            if (mapping.columns().stream().anyMatch(c -> c.kind() == Column.Kind.PASSWORD)) {
                continue;
            }
            List<CobolRecord> records = new ArrayList<>();
            String order = mapping.keyColumns().stream()
                    .map(c -> c.kind() == Column.Kind.TEXT ? c.name() + " COLLATE \"C\"" : c.name())
                    .collect(Collectors.joining(", "));
            try (Statement statement = connection.createStatement();
                 ResultSet rs = statement.executeQuery(mapping.selectSql() + " ORDER BY " + order)) {
                while (rs.next()) {
                    CobolRecord record = CobolRecord.initialized(mapping.dataset().layout(), CodePage.ASCII);
                    for (int i = 0; i < mapping.columns().size(); i++) {
                        Column column = mapping.columns().get(i);
                        FieldValues.toField(record, column, FieldValues.fromColumn(rs, i + 1, column));
                    }
                    records.add(record);
                }
            }
            RecordWriter.writeAll(dir.resolve(mapping.dataset().name()), records, RecordFormat.FIXED);
            written.put(mapping, records.size());
        }
        return written;
    }
}
