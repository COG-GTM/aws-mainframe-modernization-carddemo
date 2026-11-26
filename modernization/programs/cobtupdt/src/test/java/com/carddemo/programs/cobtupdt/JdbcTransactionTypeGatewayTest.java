package com.carddemo.programs.cobtupdt;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;

class JdbcTransactionTypeGatewayTest {

    static final String DDL = "CREATE TABLE CARDDEMO.TRANSACTION_TYPE ("
            + " TR_TYPE CHAR(2) NOT NULL, TR_DESCRIPTION VARCHAR(50) NOT NULL, PRIMARY KEY(TR_TYPE))";

    private Connection c;

    static void createSchema(Connection c) throws SQLException {
        try (Statement s = c.createStatement()) {
            s.execute("CREATE SCHEMA IF NOT EXISTS CARDDEMO");
            s.execute(DDL);
        }
    }

    static Map<String, String> rows(Connection c) throws SQLException {
        Map<String, String> m = new LinkedHashMap<>();
        try (Statement s = c.createStatement();
             ResultSet rs = s.executeQuery("SELECT TR_TYPE, TR_DESCRIPTION FROM CARDDEMO.TRANSACTION_TYPE ORDER BY TR_TYPE")) {
            while (rs.next()) {
                m.put(rs.getString(1), rs.getString(2));
            }
        }
        return m;
    }

    @BeforeEach
    void setUp() throws SQLException {
        c = DriverManager.getConnection("jdbc:h2:mem:gw" + System.nanoTime());
        createSchema(c);
    }

    @AfterEach
    void tearDown() throws SQLException {
        c.close();
    }

    @Test
    void sqlcodes() throws SQLException {
        JdbcTransactionTypeGateway g = new JdbcTransactionTypeGateway(c);
        assertEquals(0, g.insert("01", "Purchase"));
        assertEquals(-803, g.insert("01", "Purchase"));
        assertEquals(0, g.update("01", "Buy"));
        assertEquals(100, g.update("09", "x"));
        assertEquals(0, g.delete("01"));
        assertEquals(100, g.delete("01"));
        assertEquals(Map.of(), rows(c));
    }

    @Test
    void deleteOfTypeReferencedByCategoryIsRestricted() throws SQLException {
        try (Statement s = c.createStatement()) {
            s.execute("CREATE TABLE CARDDEMO.TRANSACTION_TYPE_CATEGORY (TRC_TYPE_CODE CHAR(2) NOT NULL,"
                    + " TRC_TYPE_CATEGORY CHAR(4) NOT NULL, TRC_CAT_DATA VARCHAR(50) NOT NULL,"
                    + " PRIMARY KEY(TRC_TYPE_CODE, TRC_TYPE_CATEGORY),"
                    + " FOREIGN KEY (TRC_TYPE_CODE) REFERENCES CARDDEMO.TRANSACTION_TYPE (TR_TYPE) ON DELETE RESTRICT)");
            s.execute("INSERT INTO CARDDEMO.TRANSACTION_TYPE VALUES ('01', 'Purchase')");
            s.execute("INSERT INTO CARDDEMO.TRANSACTION_TYPE_CATEGORY VALUES ('01', '0001', 'Regular')");
        }
        assertEquals(-532, new JdbcTransactionTypeGateway(c).delete("01"));
        assertEquals("Purchase", rows(c).get("01"));
    }

    @Test
    void positiveSqlcodeWarningIsReturnedInsteadOfSuccess() {
        java.sql.SQLWarning w = new java.sql.SQLWarning("info", "01000", 0);
        w.setNextWarning(new java.sql.SQLWarning("trunc", "01004", 445));
        assertEquals(445, JdbcTransactionTypeGateway.sqlcode(1, w));
        assertEquals(0, JdbcTransactionTypeGateway.sqlcode(1, new java.sql.SQLWarning("info", "01000", 0)));
        assertEquals(0, JdbcTransactionTypeGateway.sqlcode(1, null));
        assertEquals(100, JdbcTransactionTypeGateway.sqlcode(0, w));
    }

    @Test
    void negativeVendorCodePassesThroughAndOthersMapToMinusOne() {
        assertEquals(-204, JdbcTransactionTypeGateway.toSqlcode(new SQLException("x", "42704", -204)));
        assertEquals(-803, JdbcTransactionTypeGateway.toSqlcode(new SQLException("x", "23505", 23505)));
        assertEquals(-532, JdbcTransactionTypeGateway.toSqlcode(new SQLException("x", "23503", 23503)));
        assertEquals(-1, JdbcTransactionTypeGateway.toSqlcode(new SQLException("x", "42S02", 42102)));
    }

    @Test
    void programAgainstJdbcKeepsBlankPaddedVarchar() throws SQLException {
        List<String> out = new ArrayList<>();
        int rc = new Cobtupdt(FixedWidthTrRecordReader.ofLines(List.of("A01Purchase", "A01Again", "U02x")),
                new JdbcTransactionTypeGateway(c), out::add).run();
        assertEquals(4, rc);
        assertEquals("Purchase" + " ".repeat(42), rows(c).get("01"));
        assertEquals("Error accessing: TRANSACTION_TYPE table. SQLCODE: -803", out.get(6).stripTrailing());
        // WS-RETURN-MSG is never cleared: the shorter message keeps the tail of the -803 text.
        assertEquals("No records found.TRANSACTION_TYPE table. SQLCODE: -803", out.get(9).stripTrailing());
    }
}
