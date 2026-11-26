package com.carddemo.programs.cobtupdt;

import com.carddemo.programs.cobtupdt.copybook.DclTransactionType;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;

/**
 * JDBC implementation of the COBTUPDT static SQL. Commit scope is left to the caller
 * (the DSN RUN step commits once at normal program termination).
 *
 * <p>SQLCODE mapping: a negative vendor error code is returned as is (DB2 JDBC reports SQLCODE there);
 * otherwise SQLSTATE 23503/23504 (foreign key) maps to -532, the rest of class 23 to -803 and anything else to -1.
 * Zero rows on UPDATE/DELETE maps to +100.
 */
public final class JdbcTransactionTypeGateway implements TransactionTypeGateway {

    private static final String T = DclTransactionType.TABLE_NAME;
    static final String INSERT_SQL = "INSERT INTO " + T + " (TR_TYPE, TR_DESCRIPTION) VALUES (?, ?)";
    static final String UPDATE_SQL = "UPDATE " + T + " SET TR_DESCRIPTION = ? WHERE TR_TYPE = ?";
    static final String DELETE_SQL = "DELETE FROM " + T + " WHERE TR_TYPE = ?";

    private final Connection connection;

    public JdbcTransactionTypeGateway(Connection connection) {
        this.connection = connection;
    }

    @Override
    public int insert(String trType, String trDescription) {
        try (PreparedStatement ps = connection.prepareStatement(INSERT_SQL)) {
            ps.setString(1, trType);
            ps.setString(2, trDescription);
            ps.executeUpdate();
            return 0;
        } catch (SQLException e) {
            return toSqlcode(e);
        }
    }

    @Override
    public int update(String trType, String trDescription) {
        try (PreparedStatement ps = connection.prepareStatement(UPDATE_SQL)) {
            ps.setString(1, trDescription);
            ps.setString(2, trType);
            return ps.executeUpdate() == 0 ? 100 : 0;
        } catch (SQLException e) {
            return toSqlcode(e);
        }
    }

    @Override
    public int delete(String trType) {
        try (PreparedStatement ps = connection.prepareStatement(DELETE_SQL)) {
            ps.setString(1, trType);
            return ps.executeUpdate() == 0 ? 100 : 0;
        } catch (SQLException e) {
            return toSqlcode(e);
        }
    }

    /** DB2 SQLCODE for a DELETE blocked by an ON DELETE RESTRICT foreign key (TRNTYCAT). */
    public static final int SQLCODE_DELETE_RESTRICT = -532;

    static int toSqlcode(SQLException e) {
        if (e.getErrorCode() < 0) {
            return e.getErrorCode();
        }
        String state = e.getSQLState();
        if ("23503".equals(state) || "23504".equals(state)) {
            return SQLCODE_DELETE_RESTRICT;
        }
        if (state != null && state.startsWith("23")) {
            return InMemoryTransactionTypeGateway.SQLCODE_DUPLICATE_KEY;
        }
        return -1;
    }
}
