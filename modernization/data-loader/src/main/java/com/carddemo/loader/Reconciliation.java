package com.carddemo.loader;

import com.carddemo.common.record.AlphanumericField;
import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.CodePage;
import com.carddemo.common.record.Field;
import com.carddemo.common.record.NumericField;
import com.carddemo.common.record.RecordFormat;
import com.carddemo.common.record.RecordReader;
import com.carddemo.common.record.RecordWriter;
import com.carddemo.common.record.layout.AccountRecordLayout;
import com.carddemo.common.record.layout.CustomerRecordLayout;
import com.carddemo.common.record.layout.DalytranRecordLayout;
import com.carddemo.common.record.layout.DisGroupRecordLayout;
import com.carddemo.common.record.layout.SampleDataset;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import org.springframework.security.crypto.password.PasswordEncoder;

/**
 * Checks the loaded tables against the source files: row counts, and every row written back
 * through {@link RecordWriter} as a fixed-width record, read again with {@link RecordReader} and
 * compared field by field with the source record. Also cross-checks the ASCII extracts against the
 * EBCDIC datasets and lists the known source-data facts. {@link #toMarkdown()} renders the report
 * kept under {@code docs/migration/validation/}.
 */
public final class Reconciliation {

    /** Result of one dataset. {@code differences} lists count, key and field mismatches; empty when reconciled. */
    public record DatasetResult(TableMapping mapping, String source, int sourceRecords, long rows,
                                List<String> differences, boolean bytesIdentical, List<String> droppedFields,
                                int passwordsVerified) {

        public boolean reconciled() {
            return sourceRecords == rows && differences.isEmpty();
        }
    }

    private final List<DatasetResult> datasets;
    private final Map<SampleDataset, Integer> ebcdicRecordsCompared;
    private final List<String> ebcdicDifferences;
    private final List<String> sourceFacts;

    private Reconciliation(List<DatasetResult> datasets, Map<SampleDataset, Integer> ebcdicRecordsCompared,
                           List<String> ebcdicDifferences, List<String> sourceFacts) {
        this.datasets = datasets;
        this.ebcdicRecordsCompared = ebcdicRecordsCompared;
        this.ebcdicDifferences = ebcdicDifferences;
        this.sourceFacts = sourceFacts;
    }

    public static Reconciliation run(Connection connection, Path dataDir, PasswordEncoder passwordEncoder)
            throws SQLException, IOException {
        List<DatasetResult> datasets = new ArrayList<>();
        for (TableMapping mapping : TableMapping.values()) {
            datasets.add(roundTrip(connection, mapping, SourceFile.read(dataDir, mapping.dataset()), passwordEncoder));
        }
        Map<SampleDataset, Integer> compared = new LinkedHashMap<>();
        List<String> ebcdicDifferences = new ArrayList<>();
        for (TableMapping mapping : TableMapping.values()) {
            SampleDataset dataset = mapping.dataset();
            if (dataset.asciiPath(dataDir).isPresent()) {
                compared.put(dataset, crossCheck(dataDir, dataset, ebcdicDifferences));
            }
        }
        return new Reconciliation(List.copyOf(datasets), compared, List.copyOf(ebcdicDifferences), sourceFacts(dataDir));
    }

    public List<DatasetResult> datasets() {
        return datasets;
    }

    /** Field-level differences between the ASCII and EBCDIC copies, as {@code DATASET record FIELD ascii=.. ebcdic=..}. */
    public List<String> ebcdicDifferences() {
        return ebcdicDifferences;
    }

    /** Every dataset has as many rows as source records and round-trips without a difference. */
    public boolean reconciled() {
        return datasets.stream().allMatch(DatasetResult::reconciled);
    }

    private static DatasetResult roundTrip(Connection connection, TableMapping mapping, SourceFile source,
                                           PasswordEncoder passwordEncoder) throws SQLException, IOException {
        List<Column> columns = mapping.columns();
        Map<String, Object[]> rows = new LinkedHashMap<>();
        try (Statement statement = connection.createStatement();
             ResultSet rs = statement.executeQuery(mapping.selectSql())) {
            while (rs.next()) {
                Object[] values = new Object[columns.size()];
                for (int i = 0; i < columns.size(); i++) {
                    values[i] = FieldValues.fromColumn(rs, i + 1, columns.get(i));
                }
                rows.put(key(mapping, values), values);
            }
        }
        long rowCount = rows.size();

        List<String> differences = new ArrayList<>();
        List<CobolRecord> rebuilt = new ArrayList<>();
        Set<Integer> missing = new LinkedHashSet<>();
        int passwordsVerified = 0;
        for (int n = 0; n < source.records().size(); n++) {
            CobolRecord record = source.records().get(n);
            Object[] sourceValues = columns.stream().map(c -> FieldValues.toColumn(record, c)).toArray();
            String key = key(mapping, sourceValues);
            Object[] row = rows.remove(key);
            CobolRecord copy = CobolRecord.initialized(record.layout(), source.codePage());
            if (row == null) {
                differences.add("record " + (n + 1) + " key " + key + " has no row in " + mapping.table());
                missing.add(n);
            } else {
                for (int i = 0; i < columns.size(); i++) {
                    Column column = columns.get(i);
                    if (column.kind() == Column.Kind.PASSWORD) {
                        String plain = FieldValues.plainPassword(record, column);
                        if (row[i] != null && passwordEncoder.matches(plain, (String) row[i])) {
                            FieldValues.toField(copy, column, plain);
                            passwordsVerified++;
                        }
                    } else {
                        FieldValues.toField(copy, column, row[i]);
                    }
                }
            }
            rebuilt.add(copy);
        }
        rows.keySet().forEach(key -> differences.add("row key " + key + " of " + mapping.table() + " is not in the source"));

        byte[] written = write(rebuilt, source);
        SourceFile reread = SourceFile.read(source.dataset(), source.path(), source.codePage(), source.format(),
                new ByteArrayInputStream(written));
        if (reread.records().size() != source.records().size()) {
            differences.add("re-written file has " + reread.records().size() + " records, source has "
                    + source.records().size());
        } else {
            List<Field> dropped = mapping.droppedFields();
            for (int n = 0; n < source.records().size(); n++) {
                if (missing.contains(n)) {
                    continue;
                }
                Map<Field, Object> expected = source.records().get(n).values();
                Map<Field, Object> actual = reread.records().get(n).values();
                for (Field field : source.dataset().layout().fields()) {
                    if (!dropped.contains(field) && !sameValue(expected.get(field), actual.get(field))) {
                        differences.add("record " + (n + 1) + " " + field.name() + " source=" + show(expected.get(field))
                                + " round-trip=" + show(actual.get(field)));
                    }
                }
            }
        }
        boolean bytesIdentical = Arrays.equals(written, Files.readAllBytes(source.path()));
        return new DatasetResult(mapping, source.displayName(), source.records().size(), rowCount,
                List.copyOf(differences), bytesIdentical, droppedFieldNotes(mapping, source), passwordsVerified);
    }

    private static byte[] write(List<CobolRecord> records, SourceFile source) throws IOException {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        try (RecordWriter writer = new RecordWriter(out, source.format())) {
            for (int n = 0; n < records.size(); n++) {
                if (source.format().organization() == RecordFormat.Organization.FIXED) {
                    writer.write(records.get(n));
                } else {
                    writer.write(records.get(n), source.terminators().get(n));
                }
            }
        }
        return out.toByteArray();
    }

    private static String key(TableMapping mapping, Object[] values) {
        List<String> parts = new ArrayList<>();
        for (int i = 0; i < mapping.columns().size(); i++) {
            if (mapping.columns().get(i).key()) {
                parts.add(String.valueOf(values[i]));
            }
        }
        return String.join("|", parts);
    }

    /** What the source holds in each field that has no column, e.g. {@code FILLER X(22): '0' x 22 in 50 of 50 records}. */
    private static List<String> droppedFieldNotes(TableMapping mapping, SourceFile source) {
        List<String> notes = new ArrayList<>();
        for (Field field : mapping.droppedFields()) {
            Set<String> contents = new LinkedHashSet<>();
            int nonBlank = 0;
            for (CobolRecord record : source.records()) {
                String text = record.get((AlphanumericField) field);
                if (!text.isBlank()) {
                    nonBlank++;
                    contents.add(describe(text));
                }
            }
            String picture = field.name() + " X(" + field.length() + ")";
            notes.add(nonBlank == 0
                    ? picture + ": blank in all " + source.records().size() + " records"
                    : picture + ": " + String.join(" / ", contents) + " in " + nonBlank + " of "
                            + source.records().size() + " records");
        }
        return notes;
    }

    private static String describe(String text) {
        if (text.chars().distinct().count() == 1) {
            return "'" + text.charAt(0) + "' x " + text.length();
        }
        return "'" + text.stripTrailing() + "'";
    }

    private static int crossCheck(Path dataDir, SampleDataset dataset, List<String> differences) throws IOException {
        List<CobolRecord> ascii = SourceFile.read(dataDir, dataset).records();
        List<CobolRecord> ebcdic = SourceFile.readEbcdic(dataDir, dataset).records();
        if (ascii.size() != ebcdic.size()) {
            differences.add(dataset + " ascii has " + ascii.size() + " records, ebcdic " + ebcdic.size());
            return Math.min(ascii.size(), ebcdic.size());
        }
        for (int n = 0; n < ascii.size(); n++) {
            Map<Field, Object> a = ascii.get(n).values();
            Map<Field, Object> e = ebcdic.get(n).values();
            for (Field field : dataset.layout().fields()) {
                if (!sameValue(a.get(field), e.get(field))) {
                    differences.add(dataset + " record " + (n + 1) + " " + field.name() + " ascii=" + show(a.get(field))
                            + " ebcdic=" + show(e.get(field)));
                }
            }
        }
        return ascii.size();
    }

    private static List<String> sourceFacts(Path dataDir) throws IOException {
        List<String> facts = new ArrayList<>();
        List<CobolRecord> accounts = SourceFile.read(dataDir, SampleDataset.ACCTDATA).records();
        long zip = accounts.stream()
                .filter(r -> "A000000000".equals(r.getTrimmed(AccountRecordLayout.ACCT_ADDR_ZIP))).count();
        long noGroup = accounts.stream().filter(r -> r.getTrimmed(AccountRecordLayout.ACCT_GROUP_ID) == null).count();
        facts.add("`acctdata.txt`: `ACCT-ADDR-ZIP` holds `A000000000` in " + zip + " of " + accounts.size()
                + " records (record 49 of `ACCTDATA.PS` holds `ZEROAPR` there instead); loaded into `account.accounts.addr_zip` as is.");
        facts.add("`acctdata.txt`: `ACCT-GROUP-ID` is blank in " + noGroup + " of " + accounts.size()
                + " records, so `account.accounts.group_id` is null and interest calculation (CBACT04C) uses the `DEFAULT` disclosure group.");

        List<CobolRecord> groups = SourceFile.read(dataDir, SampleDataset.DISCGRP).records();
        CobolRecord group34 = groups.get(33);
        facts.add("`discgrp.txt` record 34 (`" + group34.getTrimmed(DisGroupRecordLayout.DIS_ACCT_GROUP_ID) + "` / "
                + group34.get(DisGroupRecordLayout.DIS_TRAN_TYPE_CD) + " / "
                + group34.getInt(DisGroupRecordLayout.DIS_TRAN_CAT_CD) + "): `DIS-INT-RATE` = "
                + group34.get(DisGroupRecordLayout.DIS_INT_RATE).toPlainString()
                + " (`DISCGRP.PS` has 15.00); the ASCII value is loaded.");

        List<CobolRecord> customers = SourceFile.read(dataDir, SampleDataset.CUSTDATA).records();
        long lowFico = customers.stream()
                .filter(r -> r.getInt(CustomerRecordLayout.CUST_FICO_CREDIT_SCORE) < 300).count();
        facts.add("`custdata.txt`: " + lowFico + " of " + customers.size()
                + " customers have `CUST-FICO-CREDIT-SCORE` < 300; the column check is the picture range 0..999 (`V3__fico_picture_range.sql`).");

        byte[] acctdata = Files.readAllBytes(SampleDataset.ACCTDATA.ebcdicPath(dataDir));
        byte[] accdata = Files.readAllBytes(dataDir.resolve("EBCDIC").resolve("AWS.M2.CARDDEMO.ACCDATA.PS"));
        facts.add("`ACCDATA.PS` is " + (Arrays.equals(acctdata, accdata) ? "a byte-identical copy" : "NOT a copy")
                + " of `ACCTDATA.PS`; not loaded separately.");

        Path init = dataDir.resolve("EBCDIC").resolve("AWS.M2.CARDDEMO.DALYTRAN.PS.INIT");
        List<CobolRecord> initRecords = RecordReader.readAll(init, DalytranRecordLayout.LAYOUT, CodePage.EBCDIC,
                RecordFormat.FIXED);
        List<String> set = new ArrayList<>();
        for (CobolRecord record : initRecords) {
            byte[] bytes = record.toBytes();
            for (Field field : DalytranRecordLayout.LAYOUT.fields()) {
                byte[] slice = Arrays.copyOfRange(bytes, field.offset(), field.offset() + field.length());
                boolean lowValues = true;
                for (byte b : slice) {
                    lowValues &= b == 0;
                }
                if (!lowValues) {
                    set.add("`" + field.name() + "` = '" + CodePage.EBCDIC.decodeText(slice, 0, slice.length) + "'");
                }
            }
        }
        facts.add("`DALYTRAN.PS.INIT` has " + initRecords.size() + " record(s) of low-values (X'00')"
                + (set.isEmpty() ? "" : " except " + String.join(", ", set))
                + "; it is the empty seed of TRANSACT (`TRANFILE.jcl`), not DALYTRAN input, and is not loaded.");
        return List.copyOf(facts);
    }

    private static boolean sameValue(Object a, Object b) {
        if (a instanceof BigDecimal x && b instanceof BigDecimal y) {
            return x.compareTo(y) == 0;
        }
        return Objects.equals(a, b);
    }

    private static String show(Object value) {
        return value instanceof String s ? "'" + s + "'" : String.valueOf(value);
    }

    /** One-line-per-table summary for the loader's console output. */
    public String summary() {
        StringBuilder out = new StringBuilder();
        for (DatasetResult result : datasets) {
            out.append(String.format("%-36s %-40s source %4d  rows %4d  differences %d  %s%n",
                    result.mapping().table(), result.source(), result.sourceRecords(), result.rows(),
                    result.differences().size(), result.reconciled() ? "OK" : "MISMATCH"));
            result.differences().stream().limit(20).forEach(d -> out.append("    ").append(d).append('\n'));
        }
        return out.toString();
    }

    public String toMarkdown() {
        StringBuilder md = new StringBuilder();
        md.append("""
                # Sample data reconciliation

                Generated by `SampleDataReconciliationTest` (module `data-loader`, run by `mvn -B verify` in CI):
                the services' Flyway migrations are applied to an empty PostgreSQL 16, every target table is
                checked to be empty, `SampleDataLoader` loads `app/data`, and `Reconciliation` compares the
                tables with the source files. The test fails if any count or field differs, or if this file is
                not what it generates. Regenerate after a change with
                `mvn -B -pl data-loader -am verify -Dreconciliation.write=true`. The same check runs against
                the compose database after `docker compose run --rm data-loader` (see `modernization/README.md`).

                Sources: the ASCII extracts in `app/data/ASCII`; `USRSEC`, which has no ASCII copy, from the
                EBCDIC (CP037) dataset `app/data/EBCDIC/AWS.M2.CARDDEMO.USRSEC.PS`.

                """);
        md.append("Result: **").append(reconciled() ? "reconciled" : "NOT reconciled")
                .append("**: every row count equals its source and ")
                .append(datasets.stream().mapToInt(d -> d.differences().size()).sum())
                .append(" field differences.\n\n");

        md.append("## Row counts\n\n| Dataset | Source file | Table | Source records | Rows | Match |\n|---|---|---|---|---|---|\n");
        for (DatasetResult r : datasets) {
            md.append("| ").append(r.mapping().dataset()).append(" | `").append(r.source()).append("` | `")
                    .append(r.mapping().table()).append("` | ").append(r.sourceRecords()).append(" | ").append(r.rows())
                    .append(" | ").append(r.sourceRecords() == r.rows() ? "yes" : "**no**").append(" |\n");
        }

        md.append("""

                ## Round trip

                Each row is written back into its copybook record with `RecordWriter` (same code page, record
                format and per-line terminators as the source, so CRLF/LF mixes and the 36-byte `cardxref.txt`
                lines are reproduced), the written bytes are read again with `RecordReader`, and every mapped
                field is compared with the source record (text exactly, numbers by value). FILLER has no column
                (schema-mapping.md) and is written back as spaces, so it is listed instead of compared.

                | Dataset | Records compared | Field differences | Re-written file byte-identical | Fields without a column |
                |---|---|---|---|---|
                """);
        for (DatasetResult r : datasets) {
            md.append("| ").append(r.mapping().dataset()).append(" | ").append(r.sourceRecords()).append(" | ")
                    .append(r.differences().size()).append(" | ").append(r.bytesIdentical() ? "yes" : "no").append(" | ")
                    .append(String.join("; ", r.droppedFields())).append(" |\n");
        }
        md.append("\nField-level differences: ");
        List<String> all = datasets.stream()
                .flatMap(r -> r.differences().stream().map(d -> r.mapping().dataset() + " " + d)).toList();
        if (all.isEmpty()) {
            md.append("**none**.\n");
        } else {
            md.append("\n\n");
            all.forEach(d -> md.append("- ").append(d).append('\n'));
        }
        md.append("\nA re-written file differs from the source only where a field without a column holds")
                .append(" something other than spaces (the `'0'` FILLER of the reference files).\n");

        DatasetResult users = datasets.stream().filter(r -> r.mapping() == TableMapping.USERS).findFirst().orElseThrow();
        md.append("\n## Passwords\n\n`SEC-USR-PWD` is plain text in USRSEC. The loader stores only its BCrypt hash in ")
                .append("`auth.users.password_hash` (the auth-service convention); the plain value is not persisted ")
                .append("anywhere. The round trip checks it with `BCryptPasswordEncoder.matches(source value, password_hash)`: ")
                .append(users.passwordsVerified()).append(" of ").append(users.sourceRecords()).append(" match.\n");

        md.append("\n## EBCDIC cross-check\n\nEach ASCII extract decoded with its copybook layout against the EBCDIC (CP037) ")
                .append("`.PS` dataset of the same name, every field of every record (FILLER included):\n\n")
                .append("| Dataset | Records compared |\n|---|---|\n");
        ebcdicRecordsCompared.forEach((dataset, n) -> md.append("| ").append(dataset).append(" | ").append(n).append(" |\n"));
        md.append("\nDifferences (").append(ebcdicDifferences.size()).append("), both in the source files, not decoding errors:\n\n");
        ebcdicDifferences.forEach(d -> md.append("- ").append(d).append('\n'));

        md.append("\n## Source-data facts, loaded as is\n\n");
        sourceFacts.forEach(f -> md.append("- ").append(f).append('\n'));

        md.append("""

                ## Conversions

                - `PIC X(n)` → `VARCHAR(n)`: blank padding trimmed, all blanks → null (`CobolValues.trim`).
                - `PIC 9(n)` → `BIGINT` / `INTEGER`; zoned `S9(n)V99` with sign overpunch → `NUMERIC` (`CobolRecord`).
                - `PIC X(10)` dates → `DATE`, `PIC X(26)` `YYYY-MM-DD HH:MM:SS.ffffff` → `TIMESTAMP`; a non-blank
                  value that does not parse stops the load instead of becoming null.
                - USRSEC: EBCDIC CP037 decoding of fixed 80-byte records; `SEC-USR-PWD` → BCrypt hash.
                - No sample dataset uses COMP-3 or binary fields; those decoders are covered in `common`
                  (`CobolRecordTest`, `ExportRecordLayoutTest`) for the EXPORT file, which is not loaded.
                """);
        return md.toString();
    }
}
