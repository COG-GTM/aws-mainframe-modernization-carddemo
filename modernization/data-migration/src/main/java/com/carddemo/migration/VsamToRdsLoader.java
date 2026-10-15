package com.carddemo.migration;

import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Types;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;

/**
 * Loads VSAM unloads (EBCDIC REPRO output or the ASCII copies in app/data/ASCII) into the relational schema.
 *
 * <p>Usage: {@code java -jar carddemo-data-migration.jar --jdbc-url=jdbc:postgresql://host/carddemo --user=carddemo
 * --data-dir=app/data --encoding=EBCDIC [--truncate] [--allow-missing-transact]}. The password is read from {@code CARDDEMO_DB_PASSWORD}.
 */
public final class VsamToRdsLoader {

    /** Load order honours foreign keys (customer/account before card and card_xref). */
    public record Source(RecordLayout layout, String asciiFile, String ebcdicFile, boolean optional) {
        Source(RecordLayout layout, String asciiFile, String ebcdicFile) {
            this(layout, asciiFile, ebcdicFile, false);
        }
    }

    public static final List<Source> PLAN = List.of(
            new Source(CopybookLayouts.TRANSACTION_TYPE, "trantype.txt", "AWS.M2.CARDDEMO.TRANTYPE.PS"),
            new Source(CopybookLayouts.TRANSACTION_CATEGORY, "trancatg.txt", "AWS.M2.CARDDEMO.TRANCATG.PS"),
            new Source(CopybookLayouts.DISCLOSURE_GROUP, "discgrp.txt", "AWS.M2.CARDDEMO.DISCGRP.PS"),
            new Source(CopybookLayouts.CUSTOMER, "custdata.txt", "AWS.M2.CARDDEMO.CUSTDATA.PS"),
            new Source(CopybookLayouts.ACCOUNT, "acctdata.txt", "AWS.M2.CARDDEMO.ACCTDATA.PS"),
            new Source(CopybookLayouts.CARD, "carddata.txt", "AWS.M2.CARDDEMO.CARDDATA.PS"),
            new Source(CopybookLayouts.CARD_XREF, "cardxref.txt", "AWS.M2.CARDDEMO.CARDXREF.PS"),
            new Source(CopybookLayouts.TRAN_CAT_BALANCE, "tcatbal.txt", "AWS.M2.CARDDEMO.TCATBALF.PS"),
            // TRANSACT history: required at a real cutover, optional here because CardDemo ships no TRANSACT unload.
            new Source(CopybookLayouts.TRANSACTION, "transact.txt", "AWS.M2.CARDDEMO.TRANSACT.PS", true),
            new Source(CopybookLayouts.DAILY_TRANSACTION, "dailytran.txt", "AWS.M2.CARDDEMO.DALYTRAN.PS"),
            new Source(CopybookLayouts.USER_SECURITY, null, "AWS.M2.CARDDEMO.USRSEC.PS"));

    private static final List<String> TRUNCATE_ORDER = List.of("daily_transaction_reject", "daily_transaction",
            "card_transaction", "tran_cat_balance", "card_xref", "card", "account", "customer", "disclosure_group",
            "transaction_category", "transaction_type", "user_security", "report_request");

    private final BCryptPasswordEncoder passwordEncoder = new BCryptPasswordEncoder();

    public static void main(String[] args) throws Exception {
        Map<String, String> opts = new HashMap<>();
        for (String arg : args) {
            if (arg.startsWith("--")) {
                String[] kv = arg.substring(2).split("=", 2);
                opts.put(kv[0], kv.length > 1 ? kv[1] : "true");
            }
        }
        String url = require(opts, "jdbc-url");
        String user = opts.getOrDefault("user", "carddemo");
        String password = System.getenv().getOrDefault("CARDDEMO_DB_PASSWORD", "");
        Path dataDir = Path.of(opts.getOrDefault("data-dir", "app/data"));
        SourceEncoding encoding = SourceEncoding.valueOf(opts.getOrDefault("encoding", "EBCDIC").toUpperCase());
        try (Connection connection = DriverManager.getConnection(url, user, password)) {
            VsamToRdsLoader loader = new VsamToRdsLoader();
            boolean truncate = Boolean.parseBoolean(opts.getOrDefault("truncate", "false"));
            boolean allowMissing = Boolean.parseBoolean(opts.getOrDefault("allow-missing-transact", "false"));
            System.out.print(loader.load(connection, dataDir, encoding, truncate, allowMissing));
        }
    }

    private static String require(Map<String, String> opts, String key) {
        String value = opts.get(key);
        if (value == null) {
            throw new IllegalArgumentException("--" + key + " is required");
        }
        return value;
    }

    public void truncate(Connection connection) throws SQLException {
        try (Statement st = connection.createStatement()) {
            for (String table : TRUNCATE_ORDER) {
                st.executeUpdate("DELETE FROM " + table);
            }
        }
    }

    public LoadReport load(Connection connection, Path dataDir, SourceEncoding encoding)
            throws SQLException, IOException {
        return load(connection, dataDir, encoding, false);
    }

    /** Deletes (when {@code truncate}) and reloads in one transaction, so a failed reload keeps the previous data. */
    public LoadReport load(Connection connection, Path dataDir, SourceEncoding encoding, boolean truncate)
            throws SQLException, IOException {
        return load(connection, dataDir, encoding, truncate, false);
    }

    /**
     * @param allowMissingOptional sample-data mode: lets a truncating reload proceed without optional unloads
     *     (TRANSACT). Without it, a truncating load requires every unload so history is never silently dropped.
     */
    public LoadReport load(Connection connection, Path dataDir, SourceEncoding encoding, boolean truncate,
                           boolean allowMissingOptional) throws SQLException, IOException {
        for (Source source : PLAN) {
            boolean required = !source.optional() || (truncate && !allowMissingOptional);
            if (required && !available(dataDir, source, encoding)) {
                throw new IOException("Missing unload for " + source.layout().table() + " in " + dataDir);
            }
        }
        boolean autoCommit = connection.getAutoCommit();
        connection.setAutoCommit(false);
        LoadReport report = new LoadReport();
        try {
            if (truncate) {
                truncate(connection);
            }
            for (Source source : PLAN) {
                if (source.optional() && !available(dataDir, source, encoding)) {
                    continue;
                }
                List<byte[]> records;
                try (InputStream in = open(dataDir, source, encoding)) {
                    records = RecordReader.read(in, source.layout().recordLength(), encoding);
                }
                List<Map<String, Object>> rows = new ArrayList<>(records.size());
                for (byte[] record : records) {
                    rows.add(transform(source.layout(), RecordDecoder.decode(record, source.layout(), encoding)));
                }
                insert(connection, source.layout().table(), rows);
                report.count(source.layout().table(), rows.size());
                controlTotals(source.layout(), rows, report);
            }
            resetTransactionSequence(connection);
            connection.commit();
            return report;
        } catch (SQLException | IOException | RuntimeException e) {
            connection.rollback();
            throw e;
        } finally {
            connection.setAutoCommit(autoCommit);
        }
    }

    private static boolean available(Path dataDir, Source source, SourceEncoding encoding) {
        if (encoding == SourceEncoding.ASCII) {
            return source.asciiFile() == null || Files.exists(dataDir.resolve("ASCII").resolve(source.asciiFile()));
        }
        return Files.exists(dataDir.resolve("EBCDIC").resolve(source.ebcdicFile()));
    }

    private InputStream open(Path dataDir, Source source, SourceEncoding encoding) throws IOException {
        if (encoding == SourceEncoding.ASCII) {
            if (source.asciiFile() == null) {
                // No ASCII copy of USRSEC ships with CardDemo; use the DUSRSECJ.jcl in-stream records.
                InputStream seed = VsamToRdsLoader.class.getResourceAsStream("/seed/usrsec.txt");
                if (seed == null) {
                    throw new IOException("Missing seed/usrsec.txt resource");
                }
                return seed;
            }
            return Files.newInputStream(dataDir.resolve("ASCII").resolve(source.asciiFile()));
        }
        return Files.newInputStream(dataDir.resolve("EBCDIC").resolve(source.ebcdicFile()));
    }

    private Map<String, Object> transform(RecordLayout layout, Map<String, Object> row) {
        if (layout == CopybookLayouts.USER_SECURITY) {
            String clear = (String) row.get("password_hash");
            row.put("password_hash", passwordEncoder.encode(clear == null ? "" : clear.trim().toUpperCase(java.util.Locale.ROOT)));
        }
        if (layout == CopybookLayouts.DAILY_TRANSACTION) {
            row.put("status", "PENDING");
        }
        return row;
    }

    private static void insert(Connection connection, String table, List<Map<String, Object>> rows)
            throws SQLException {
        if (rows.isEmpty()) {
            return;
        }
        List<String> columns = new ArrayList<>(rows.get(0).keySet());
        String sql = "INSERT INTO " + table + " (" + String.join(", ", columns) + ") VALUES ("
                + String.join(", ", columns.stream().map(c -> "?").toList()) + ")";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            for (Map<String, Object> row : rows) {
                for (int i = 0; i < columns.size(); i++) {
                    bind(ps, i + 1, row.get(columns.get(i)));
                }
                ps.addBatch();
            }
            ps.executeBatch();
        }
    }

    private static void bind(PreparedStatement ps, int index, Object value) throws SQLException {
        if (value == null) {
            ps.setNull(index, Types.NULL);
        } else if (value instanceof LocalDate d) {
            ps.setObject(index, d, Types.DATE);
        } else if (value instanceof LocalDateTime t) {
            ps.setObject(index, t, Types.TIMESTAMP);
        } else {
            ps.setObject(index, value);
        }
    }

    private static void controlTotals(RecordLayout layout, List<Map<String, Object>> rows, LoadReport report) {
        String column = layout == CopybookLayouts.ACCOUNT ? "curr_bal"
                : layout == CopybookLayouts.TRAN_CAT_BALANCE ? "balance"
                : layout == CopybookLayouts.DAILY_TRANSACTION ? "amount" : null;
        if (column == null) {
            return;
        }
        BigDecimal sum = rows.stream().map(r -> (BigDecimal) r.get(column)).reduce(BigDecimal.ZERO, BigDecimal::add);
        report.total(layout.table() + "." + column, sum);
    }

    /** Positions tran_id_seq past every numeric TRAN-ID already present (posted or pending). */
    static void resetTransactionSequence(Connection connection) throws SQLException {
        long max = 0;
        for (String table : List.of("card_transaction", "daily_transaction")) {
            try (Statement st = connection.createStatement();
                 ResultSet rs = st.executeQuery("SELECT tran_id FROM " + table)) {
                while (rs.next()) {
                    String id = rs.getString(1).trim();
                    if (id.chars().allMatch(Character::isDigit)) {
                        max = Math.max(max, Long.parseLong(id));
                    }
                }
            }
        }
        try (Statement st = connection.createStatement()) {
            st.execute("ALTER SEQUENCE tran_id_seq RESTART WITH " + (max + 1));
        }
    }
}
