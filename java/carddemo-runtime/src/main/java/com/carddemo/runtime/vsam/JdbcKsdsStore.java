package com.carddemo.runtime.vsam;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import javax.sql.DataSource;

/**
 * {@link KsdsStore} persisted in a relational table (H2 in tests and the default online profile):
 * {@code VSAM_KSDS(DATASET, REC_KEY VARBINARY, REC_DATA VARBINARY)}, keyed by (DATASET, REC_KEY).
 * Key order relies on the database comparing VARBINARY as unsigned bytes (true for H2).
 */
public final class JdbcKsdsStore implements KsdsStore {

    private static final String TABLE = "VSAM_KSDS";

    private final DataSource dataSource;
    private final String dataset;

    public JdbcKsdsStore(DataSource dataSource, String dataset) {
        this.dataSource = dataSource;
        this.dataset = dataset;
        execute("CREATE TABLE IF NOT EXISTS " + TABLE + " (DATASET VARCHAR(44) NOT NULL, REC_KEY VARBINARY(255) NOT NULL,"
                + " REC_DATA VARBINARY(32767) NOT NULL, PRIMARY KEY (DATASET, REC_KEY))");
    }

    @Override
    public Optional<byte[]> get(byte[] key) {
        return one("SELECT REC_KEY, REC_DATA FROM " + TABLE + " WHERE DATASET = ? AND REC_KEY = ?", key)
                .map(Map.Entry::getValue);
    }

    @Override
    public boolean insert(byte[] key, byte[] record) {
        if (get(key).isPresent()) {
            return false;
        }
        return update("INSERT INTO " + TABLE + " (DATASET, REC_KEY, REC_DATA) VALUES (?, ?, ?)", dataset, key, record) == 1;
    }

    @Override
    public boolean update(byte[] key, byte[] record) {
        return update("UPDATE " + TABLE + " SET REC_DATA = ? WHERE DATASET = ? AND REC_KEY = ?", record, dataset, key) == 1;
    }

    @Override
    public boolean delete(byte[] key) {
        return update("DELETE FROM " + TABLE + " WHERE DATASET = ? AND REC_KEY = ?", dataset, key) == 1;
    }

    @Override
    public Optional<Map.Entry<byte[], byte[]>> ceiling(byte[] key) {
        return one(select(">=", "ASC"), key);
    }

    @Override
    public Optional<Map.Entry<byte[], byte[]>> higher(byte[] key) {
        return one(select(">", "ASC"), key);
    }

    @Override
    public Optional<Map.Entry<byte[], byte[]>> lower(byte[] key) {
        return one(select("<", "DESC"), key);
    }

    @Override
    public Optional<Map.Entry<byte[], byte[]>> last() {
        List<Map.Entry<byte[], byte[]>> rows = query("SELECT REC_KEY, REC_DATA FROM " + TABLE
                + " WHERE DATASET = ? ORDER BY REC_KEY DESC FETCH FIRST 1 ROWS ONLY");
        return rows.stream().findFirst();
    }

    @Override
    public List<Map.Entry<byte[], byte[]>> all() {
        return query("SELECT REC_KEY, REC_DATA FROM " + TABLE + " WHERE DATASET = ? ORDER BY REC_KEY");
    }

    @Override
    public long count() {
        return all().size();
    }

    @Override
    public void clear() {
        update("DELETE FROM " + TABLE + " WHERE DATASET = ?", dataset);
    }

    private static String select(String op, String order) {
        return "SELECT REC_KEY, REC_DATA FROM " + TABLE + " WHERE DATASET = ? AND REC_KEY " + op
                + " ? ORDER BY REC_KEY " + order + " FETCH FIRST 1 ROWS ONLY";
    }

    private Optional<Map.Entry<byte[], byte[]>> one(String sql, byte[] key) {
        try (Connection c = dataSource.getConnection(); PreparedStatement ps = c.prepareStatement(sql)) {
            ps.setString(1, dataset);
            ps.setBytes(2, key);
            try (ResultSet rs = ps.executeQuery()) {
                return rs.next() ? Optional.of(Map.entry(rs.getBytes(1), rs.getBytes(2))) : Optional.empty();
            }
        } catch (SQLException e) {
            throw new IllegalStateException(e);
        }
    }

    private List<Map.Entry<byte[], byte[]>> query(String sql) {
        try (Connection c = dataSource.getConnection(); PreparedStatement ps = c.prepareStatement(sql)) {
            ps.setString(1, dataset);
            List<Map.Entry<byte[], byte[]>> out = new ArrayList<>();
            try (ResultSet rs = ps.executeQuery()) {
                while (rs.next()) {
                    out.add(Map.entry(rs.getBytes(1), rs.getBytes(2)));
                }
            }
            return out;
        } catch (SQLException e) {
            throw new IllegalStateException(e);
        }
    }

    private int update(String sql, Object... args) {
        try (Connection c = dataSource.getConnection(); PreparedStatement ps = c.prepareStatement(sql)) {
            for (int i = 0; i < args.length; i++) {
                if (args[i] instanceof byte[] b) {
                    ps.setBytes(i + 1, b);
                } else {
                    ps.setString(i + 1, (String) args[i]);
                }
            }
            return ps.executeUpdate();
        } catch (SQLException e) {
            throw new IllegalStateException(e);
        }
    }

    private void execute(String sql) {
        try (Connection c = dataSource.getConnection(); Statement st = c.createStatement()) {
            st.execute(sql);
        } catch (SQLException e) {
            throw new IllegalStateException(e);
        }
    }
}
