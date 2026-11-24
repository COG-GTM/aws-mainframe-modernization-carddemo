package com.carddemo.transactiontype.adapters.db2;

import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.carddemo.transactiontype.domain.SqlCodeException;
import java.sql.SQLException;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.dao.DataAccessResourceFailureException;
import org.springframework.jdbc.UncategorizedSQLException;
import org.springframework.jdbc.core.JdbcTemplate;

class JdbcTransactionTypeStoreTest {

    private final JdbcTemplate jdbc = mock(JdbcTemplate.class);
    private final JdbcTransactionTypeStore store = new JdbcTransactionTypeStore(jdbc);

    private void failWith(SQLException cause) {
        when(jdbc.queryForList(anyString(), eq(String.class)))
                .thenThrow(new UncategorizedSQLException("extract", "SELECT", cause));
    }

    @Test
    @DisplayName("BR-TT-061 JCC connection failure (SQLSTATE 08xxx / -4499) is an outage, not a statement SQLCODE")
    void br_tt_061_connectionFailureIsOutage() {
        failWith(new SQLException("connection refused", "08001", -4499));
        assertThatThrownBy(store::extractCategories).isInstanceOf(DataAccessResourceFailureException.class);
    }

    @Test
    @DisplayName("BR-TT-060 other negative SQLCODEs still surface as SqlCodeException")
    void br_tt_060_statementSqlCode() {
        failWith(new SQLException("bad", "42601", -104));
        assertThatThrownBy(store::extractCategories).isInstanceOf(SqlCodeException.class);
    }
}
