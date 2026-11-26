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
        int rc = Cobtupdt.execute(new String[] {"no/such/INPFILE"}, Map.of(), out, err);
        assertEquals(12, rc);
        assertTrue(outBytes.toString(StandardCharsets.UTF_8).startsWith("OPEN FILE NOT OK"));
        assertTrue(errBytes.toString(StandardCharsets.UTF_8).contains("FILE STATUS 47"));
    }

    @Test
    void inMemoryRunReturnsProgramReturnCode(@TempDir Path dir) throws Exception {
        Path f = dir.resolve("INPFILE");
        Files.writeString(f, "A01Purchase\nD02\n");
        int rc = Cobtupdt.execute(new String[] {f.toString()}, Map.of(), out, err);
        assertEquals(4, rc);
        assertTrue(outBytes.toString(StandardCharsets.UTF_8).contains("RECORD INSERTED SUCCESSFULLY"));
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
}
