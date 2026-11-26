package com.carddemo.programs.cobtupdt;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.DriverManager;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class MainTest {

    private final ByteArrayOutputStream outBytes = new ByteArrayOutputStream();
    private final ByteArrayOutputStream errBytes = new ByteArrayOutputStream();
    private final PrintStream out = new PrintStream(outBytes, true, StandardCharsets.UTF_8);
    private final PrintStream err = new PrintStream(errBytes, true, StandardCharsets.UTF_8);

    @Test
    void noArgumentsIsSevere() {
        assertEquals(12, Cobtupdt.execute(new String[0], Map.of(), out, err));
    }

    @Test
    void missingInputFileIsSevere() {
        int rc = Cobtupdt.execute(new String[] {"no/such/INPFILE"},
                Map.of(Cobtupdt.ENV_JDBC_URL, "jdbc:h2:mem:missing" + System.nanoTime()), out, err);
        assertEquals(12, rc);
        assertTrue(outBytes.toString(StandardCharsets.UTF_8).startsWith("OPEN FILE NOT OK"));
        assertTrue(errBytes.toString(StandardCharsets.UTF_8).contains("FILE STATUS 47"));
    }

    @Test
    void missingJdbcUrlIsSevereAndAppliesNothing(@TempDir Path dir) throws Exception {
        Path f = dir.resolve("INPFILE");
        Files.writeString(f, "A01Purchase\nD02\n");
        int rc = Cobtupdt.execute(new String[] {f.toString()}, Map.of(), out, err);
        assertEquals(12, rc);
        assertEquals("", outBytes.toString(StandardCharsets.UTF_8));
        assertTrue(errBytes.toString(StandardCharsets.UTF_8).contains(Cobtupdt.ENV_JDBC_URL + " not set"));
    }

    @Test
    void jdbcRunCommitsAtNormalTerminationEvenWithRc4(@TempDir Path dir) throws Exception {
        String url = "jdbc:h2:mem:main" + System.nanoTime() + ";DB_CLOSE_DELAY=-1";
        try (Connection c = DriverManager.getConnection(url)) {
            JdbcTransactionTypeGatewayTest.createSchema(c);
            Path f = dir.resolve("INPFILE");
            Files.writeString(f, "A01Purchase\nU99Missing\n");
            int rc = Cobtupdt.execute(new String[] {f.toString()}, Map.of(Cobtupdt.ENV_JDBC_URL, url), out, err);
            assertEquals(4, rc);
            assertEquals("Purchase" + " ".repeat(42), JdbcTransactionTypeGatewayTest.rows(c).get("01"));
        }
    }

    @Test
    void connectionFailureDoesNotPrintTheJdbcUrl(@TempDir Path dir) throws Exception {
        Path f = dir.resolve("INPFILE");
        Files.writeString(f, "A01Purchase\n");
        String url = "jdbc:nosuchdriver://db?user=u&password=s3cret";
        int rc = Cobtupdt.execute(new String[] {f.toString()}, Map.of(Cobtupdt.ENV_JDBC_URL, url), out, err);
        assertEquals(12, rc);
        String e = errBytes.toString(StandardCharsets.UTF_8);
        assertTrue(e.contains("DB2 connection failed"));
        assertTrue(!e.contains("s3cret") && !e.contains("nosuchdriver"));
    }

    @Test
    void fixedBlockLayoutAppliesEverySeparatorFreeRecord(@TempDir Path dir) throws Exception {
        String url = "jdbc:h2:mem:fb" + System.nanoTime() + ";DB_CLOSE_DELAY=-1";
        try (Connection c = DriverManager.getConnection(url)) {
            JdbcTransactionTypeGatewayTest.createSchema(c);
            Path f = dir.resolve("INPFILE");
            Files.writeString(f, TrRecord.parse("A01Purchase").toFixedWidth() + TrRecord.parse("A02Refund").toFixedWidth());
            int rc = Cobtupdt.execute(new String[] {f.toString()},
                    Map.of(Cobtupdt.ENV_JDBC_URL, url, Cobtupdt.ENV_INPFILE_LAYOUT, "FIXED_BLOCK"), out, err);
            assertEquals(0, rc);
            assertEquals(2, JdbcTransactionTypeGatewayTest.rows(c).size());
        }
    }

    @Test
    void invalidLayoutIsSevere() {
        int rc = Cobtupdt.execute(new String[] {"INPFILE"}, Map.of(Cobtupdt.ENV_INPFILE_LAYOUT, "VSAM"), out, err);
        assertEquals(12, rc);
    }

    @Test
    void fixedBlockReadsEbcdicWithIbm037(@TempDir Path dir) throws Exception {
        String url = "jdbc:h2:mem:ebc" + System.nanoTime() + ";DB_CLOSE_DELAY=-1";
        try (Connection c = DriverManager.getConnection(url)) {
            JdbcTransactionTypeGatewayTest.createSchema(c);
            Path f = dir.resolve("INPFILE");
            String data = TrRecord.parse("A01Purchase").toFixedWidth() + TrRecord.parse("A02Refund").toFixedWidth();
            Files.write(f, data.getBytes(java.nio.charset.Charset.forName("IBM037")));
            int rc = Cobtupdt.execute(new String[] {f.toString()}, Map.of(Cobtupdt.ENV_JDBC_URL, url,
                    Cobtupdt.ENV_INPFILE_LAYOUT, "FIXED_BLOCK", Cobtupdt.ENV_INPFILE_CHARSET, "IBM037"), out, err);
            assertEquals(0, rc);
            assertEquals("Refund" + " ".repeat(44), JdbcTransactionTypeGatewayTest.rows(c).get("02"));
        }
    }

    @Test
    void fixedBlockDefaultCharsetKeepsByteAlignmentWithMultibyteText(@TempDir Path dir) throws Exception {
        String url = "jdbc:h2:mem:mb" + System.nanoTime() + ";DB_CLOSE_DELAY=-1";
        try (Connection c = DriverManager.getConnection(url)) {
            JdbcTransactionTypeGatewayTest.createSchema(c);
            byte[] rec1 = ("A01Caf\u00e9" + " ".repeat(45)).getBytes(StandardCharsets.UTF_8);
            byte[] rec2 = TrRecord.parse("A02Refund").toFixedWidth().getBytes(StandardCharsets.UTF_8);
            assertEquals(53, rec1.length);
            Path f = dir.resolve("INPFILE");
            Files.write(f, java.nio.ByteBuffer.allocate(106).put(rec1).put(rec2).array());
            int rc = Cobtupdt.execute(new String[] {f.toString()},
                    Map.of(Cobtupdt.ENV_JDBC_URL, url, Cobtupdt.ENV_INPFILE_LAYOUT, "FIXED_BLOCK"), out, err);
            assertEquals(0, rc);
            assertEquals(2, JdbcTransactionTypeGatewayTest.rows(c).size());
        }
    }

    @Test
    void fixedBlockRejectsMultibyteCharset() {
        int rc = Cobtupdt.execute(new String[] {"INPFILE"}, Map.of(Cobtupdt.ENV_INPFILE_LAYOUT, "FIXED_BLOCK",
                Cobtupdt.ENV_INPFILE_CHARSET, "UTF-8"), out, err);
        assertEquals(12, rc);
        assertTrue(errBytes.toString(StandardCharsets.UTF_8).contains("single-byte"));
    }
}
