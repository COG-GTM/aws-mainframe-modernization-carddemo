package com.carddemo.batch.posttran;

import com.carddemo.transaction.TransactionRepository;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import javax.sql.DataSource;
import org.springframework.dao.DataAccessResourceFailureException;

/**
 * The online transaction-id lock ({@link TransactionRepository#TRAN_ID_LOCK}) held as a PostgreSQL session lock for a
 * whole batch step that writes {@code transaction} rows with ids taken from its input.
 *
 * <p>Online {@code TransactionIds} assigns highest id + 1 under the transaction-scoped form of the same key, so a
 * per-record lock would still let an online add take an id the step writes later (DALYTRAN ids are ascending). Held
 * for the step, online adds wait until the posting has ended, as CICS could not write TRANSACT while the batch window
 * had it closed. The lock lives on its own pooled connection; the step's units of work use other connections.
 */
final class TransactionIdStepLock implements AutoCloseable {

    private final Connection connection;

    private TransactionIdStepLock(Connection connection) {
        this.connection = connection;
    }

    static TransactionIdStepLock acquire(DataSource dataSource) {
        Connection connection = null;
        try {
            connection = dataSource.getConnection();
            connection.setAutoCommit(true);
            call(connection, "select pg_advisory_lock(?)");
            return new TransactionIdStepLock(connection);
        } catch (SQLException e) {
            closeQuietly(connection);
            throw new DataAccessResourceFailureException("cannot take the transaction id lock", e);
        }
    }

    @Override
    public void close() {
        try {
            call(connection, "select pg_advisory_unlock(?)");
        } catch (SQLException e) {
            throw new DataAccessResourceFailureException("cannot release the transaction id lock", e);
        } finally {
            closeQuietly(connection);
        }
    }

    private static void call(Connection connection, String sql) throws SQLException {
        try (PreparedStatement statement = connection.prepareStatement(sql)) {
            statement.setLong(1, TransactionRepository.TRAN_ID_LOCK);
            statement.execute();
        }
    }

    private static void closeQuietly(Connection connection) {
        if (connection == null) {
            return;
        }
        try {
            connection.close();
        } catch (SQLException ignored) {
            // the pool discards a broken connection; a session lock ends with its session
        }
    }
}
