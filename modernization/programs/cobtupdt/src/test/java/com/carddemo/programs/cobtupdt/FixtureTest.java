package com.carddemo.programs.cobtupdt;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Uses app/data/ASCII/trantype.txt (2-byte type + 50-byte description + 8-byte filler, CRLF) to build
 * an MNTTRDB2 INPFILE: column 1 = action, columns 2-53 = the first 52 bytes of each trantype record.
 */
class FixtureTest {

    private static List<String> trantype() throws IOException, URISyntaxException {
        Path p = Path.of(FixtureTest.class.getResource("/trantype.txt").toURI());
        return Files.readAllLines(p, StandardCharsets.UTF_8).stream()
                .map(l -> l.endsWith("\r") ? l.substring(0, l.length() - 1) : l)
                .toList();
    }

    @Test
    void loadsAllSevenTransactionTypesFromSampleData() throws Exception {
        List<String> inpfile = trantype().stream().map(l -> "A" + l).toList();
        InMemoryTransactionTypeGateway table = new InMemoryTransactionTypeGateway();
        List<String> out = new ArrayList<>();

        int rc = new Cobtupdt(FixedWidthTrRecordReader.ofLines(inpfile), table, out::add).run();

        assertEquals(0, rc);
        assertEquals(1 + 7 * 3, out.size());
        assertEquals("PROCESSING   A01Purchase" + " ".repeat(42), out.get(1));
        Map<String, String> rows = table.rows();
        assertEquals(List.of("01", "02", "03", "04", "05", "06", "07"), List.copyOf(rows.keySet()));
        assertEquals("Purchase", rows.get("01").stripTrailing());
        assertEquals("Authorization", rows.get("04").stripTrailing());
        assertEquals("Adjustment", rows.get("07").stripTrailing());
        rows.values().forEach(d -> assertEquals(50, d.length()));
    }

    @Test
    void reloadingSampleDataHitsDuplicateKeyOnEveryRecord() throws Exception {
        List<String> inpfile = trantype().stream().map(l -> "A" + l).toList();
        InMemoryTransactionTypeGateway table = new InMemoryTransactionTypeGateway();
        new Cobtupdt(FixedWidthTrRecordReader.ofLines(inpfile), table, s -> { }).run();
        List<String> out = new ArrayList<>();

        int rc = new Cobtupdt(FixedWidthTrRecordReader.ofLines(inpfile), table, out::add).run();

        assertEquals(4, rc);
        assertEquals(7, out.stream()
                .filter(s -> s.stripTrailing().equals("Error accessing: TRANSACTION_TYPE table. SQLCODE: -803"))
                .count());
    }

    @Test
    void readsSampleFileDirectlyFromDisk() throws Exception {
        Path p = Path.of(FixtureTest.class.getResource("/trantype.txt").toURI());
        List<String> out = new ArrayList<>();
        int rc = new Cobtupdt(FixedWidthTrRecordReader.of(p), new InMemoryTransactionTypeGateway(), out::add).run();
        // Column 1 of trantype.txt is '0', not an action code: every record is rejected.
        assertEquals(4, rc);
        assertEquals(7, out.stream().filter(s -> s.startsWith("ERROR: TYPE NOT VALID")).count());
        assertEquals("PROCESSING   01Purchase" + " ".repeat(42) + "0", out.get(1));
    }
}
