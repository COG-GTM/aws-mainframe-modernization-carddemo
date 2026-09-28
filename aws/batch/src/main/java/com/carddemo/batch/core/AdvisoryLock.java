package com.carddemo.batch.core;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Optional;
import javax.sql.DataSource;
import org.springframework.dao.DataAccessResourceFailureException;
import org.springframework.stereotype.Component;

/**
 * PostgreSQL session advisory lock held on a dedicated connection. The lock lives exactly as long as the holder's
 * database session, so a container that dies releases it immediately and a retry can proceed, while a genuinely
 * concurrent holder is excluded.
 */
@Component
public class AdvisoryLock {

    private final DataSource dataSource;

    public AdvisoryLock(DataSource dataSource) {
        this.dataSource = dataSource;
    }

    /** Non-blocking; empty when another session holds the lock for {@code (scope, key)}. */
    public Optional<Lease> tryAcquire(String scope, String key) {
        Connection c = null;
        try {
            c = dataSource.getConnection();
            c.setAutoCommit(true);
            if (call(c, "SELECT pg_try_advisory_lock(hashtext(?), hashtext(?))", scope, key)) {
                return Optional.of(new Lease(c, scope, key));
            }
            c.close();
            return Optional.empty();
        } catch (SQLException e) {
            closeQuietly(c);
            throw new DataAccessResourceFailureException("advisory lock " + scope + "/" + key, e);
        }
    }

    private static boolean call(Connection c, String sql, String scope, String key) throws SQLException {
        try (PreparedStatement ps = c.prepareStatement(sql)) {
            ps.setString(1, scope);
            ps.setString(2, key);
            try (ResultSet rs = ps.executeQuery()) {
                return rs.next() && rs.getBoolean(1);
            }
        }
    }

    private static void closeQuietly(Connection c) {
        if (c != null) {
            try {
                c.close();
            } catch (SQLException ignored) {
                // connection already unusable
            }
        }
    }

    /** Releases the lock before returning the connection to the pool (session locks survive pooling). */
    public static final class Lease implements AutoCloseable {
        private final Connection connection;
        private final String scope;
        private final String key;

        private Lease(Connection connection, String scope, String key) {
            this.connection = connection;
            this.scope = scope;
            this.key = key;
        }

        @Override
        public void close() {
            try {
                call(connection, "SELECT pg_advisory_unlock(hashtext(?), hashtext(?))", scope, key);
            } catch (SQLException e) {
                try {
                    connection.abort(Runnable::run);
                } catch (SQLException ignored) {
                    // physical close releases the session lock
                }
            } finally {
                closeQuietly(connection);
            }
        }
    }
}
