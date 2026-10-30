package com.carddemo.transactiontype.adapters.db2;

import com.carddemo.transactiontype.domain.ListFilter;
import com.carddemo.transactiontype.domain.SqlCodeException;
import com.carddemo.transactiontype.domain.TransactionType;
import com.carddemo.transactiontype.domain.TransactionTypeCategory;
import com.carddemo.transactiontype.domain.TransactionTypeStore;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Supplier;
import org.springframework.dao.DataAccessException;
import org.springframework.dao.DataAccessResourceFailureException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.stereotype.Repository;

/** Db2 (IBM JCC) adapter. SQL mirrors the embedded SQL in COTRTLIC / COTRTUPC / COBTUPDT / TRANEXTR. */
@Repository
public class JdbcTransactionTypeStore implements TransactionTypeStore {

    private static final RowMapper<TransactionType> ROW =
            (rs, i) -> new TransactionType(rs.getString("TR_TYPE"), rs.getString("TR_DESCRIPTION"));

    private final JdbcTemplate jdbc;

    public JdbcTransactionTypeStore(JdbcTemplate jdbc) {
        this.jdbc = jdbc;
    }

    @Override
    public Optional<TransactionType> findByCode(String code) {
        return sql(() -> jdbc.query(
                "SELECT TR_TYPE, TR_DESCRIPTION FROM CARDDEMO.TRANSACTION_TYPE WHERE TR_TYPE = ?", ROW, code)
                .stream().findFirst());
    }

    @Override
    public List<TransactionType> findForward(String startKey, ListFilter filter, int limit) {
        return cursor("TR_TYPE >= ?", startKey, filter, "ASC", limit);
    }

    @Override
    public List<TransactionType> findBackward(String beforeKey, ListFilter filter, int limit) {
        return cursor("TR_TYPE < ?", beforeKey, filter, "DESC", limit);
    }

    private List<TransactionType> cursor(String keyPredicate, String key, ListFilter filter, String order, int limit) {
        List<Object> args = new ArrayList<>();
        args.add(key);
        String where = keyPredicate + filterSql(filter, args);
        String q = "SELECT TR_TYPE, TR_DESCRIPTION FROM CARDDEMO.TRANSACTION_TYPE WHERE " + where
                + " ORDER BY TR_TYPE " + order + " FETCH FIRST " + limit + " ROWS ONLY";
        return sql(() -> jdbc.query(q, ROW, args.toArray()));
    }

    @Override
    public long count(ListFilter filter) {
        List<Object> args = new ArrayList<>();
        String q = "SELECT COUNT(1) FROM CARDDEMO.TRANSACTION_TYPE WHERE 1 = 1" + filterSql(filter, args);
        return sql(() -> jdbc.queryForObject(q, Long.class, args.toArray()));
    }

    private static String filterSql(ListFilter filter, List<Object> args) {
        StringBuilder sb = new StringBuilder();
        if (filter.typeCode() != null) {
            sb.append(" AND TR_TYPE = ?");
            args.add(filter.typeCode());
        }
        if (filter.descriptionPattern() != null) {
            sb.append(" AND TR_DESCRIPTION LIKE ?");
            args.add(filter.descriptionPattern());
        }
        return sb.toString();
    }

    @Override
    public void insert(TransactionType tt) {
        sql(() -> jdbc.update("INSERT INTO CARDDEMO.TRANSACTION_TYPE (TR_TYPE, TR_DESCRIPTION) VALUES (?, ?)",
                tt.code(), tt.description()));
    }

    @Override
    public int updateDescription(String code, String description) {
        return sql(() -> jdbc.update("UPDATE CARDDEMO.TRANSACTION_TYPE SET TR_DESCRIPTION = ? WHERE TR_TYPE = ?",
                description, code));
    }

    @Override
    public int delete(String code) {
        return sql(() -> jdbc.update("DELETE FROM CARDDEMO.TRANSACTION_TYPE WHERE TR_TYPE = ?", code));
    }

    @Override
    public List<TransactionTypeCategory> findCategories(String typeCode) {
        return sql(() -> jdbc.query(
                "SELECT TRC_TYPE_CODE, TRC_TYPE_CATEGORY, TRC_CAT_DATA FROM CARDDEMO.TRANSACTION_TYPE_CATEGORY "
                        + "WHERE TRC_TYPE_CODE = ? ORDER BY TRC_TYPE_CATEGORY",
                (rs, i) -> new TransactionTypeCategory(rs.getString(1), rs.getString(2), rs.getString(3)), typeCode));
    }

    @Override
    public List<String> extractTransactionTypes() {
        return sql(() -> jdbc.queryForList(
                "SELECT CAST(CONCAT(CONCAT(TR_TYPE, CAST(TR_DESCRIPTION AS CHAR(50))), REPEAT('0', 8)) AS CHAR(60)) "
                        + "FROM CARDDEMO.TRANSACTION_TYPE ORDER BY TR_TYPE", String.class));
    }

    @Override
    public List<String> extractCategories() {
        return sql(() -> jdbc.queryForList(
                "SELECT CAST(TRC_TYPE_CODE || TRC_TYPE_CATEGORY || CAST(TRC_CAT_DATA AS CHAR(50)) || REPEAT('0', 4) "
                        + "AS CHAR(60)) FROM CARDDEMO.TRANSACTION_TYPE_CATEGORY "
                        + "ORDER BY TRC_TYPE_CODE, TRC_TYPE_CATEGORY", String.class));
    }

    /** Surfaces the Db2 SQLCODE (JCC reports it as SQLException#getErrorCode) like the SQLCA does. */
    private static <T> T sql(Supplier<T> call) {
        try {
            return call.get();
        } catch (DataAccessResourceFailureException e) {
            throw e;
        } catch (DataAccessException e) {
            for (Throwable t = e; t != null; t = t.getCause()) {
                if (t instanceof SQLException se && isConnectionFailure(se)) {
                    throw new DataAccessResourceFailureException(se.getMessage(), e);
                }
            }
            for (Throwable t = e; t != null; t = t.getCause()) {
                if (t instanceof SQLException se && se.getErrorCode() != 0) {
                    throw new SqlCodeException(se.getErrorCode(), se.getMessage(), e);
                }
            }
            throw e;
        }
    }

    /** BR-TT-061: SQLSTATE class 08 or JCC -4499 means Db2 is unreachable, not a statement error. */
    private static boolean isConnectionFailure(SQLException se) {
        String state = se.getSQLState();
        return se.getErrorCode() == -4499 || (state != null && state.startsWith("08"));
    }
}
