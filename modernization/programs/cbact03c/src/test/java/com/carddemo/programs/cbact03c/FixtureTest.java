package com.carddemo.programs.cbact03c;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.net.URISyntaxException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Fixture tests against app/data sample files. The expected SYSOUT was captured by compiling the
 * unmodified app/cbl/CBACT03C.cbl with GnuCOBOL 3 (cobc -std=ibm) and running it over an indexed
 * XREFFILE loaded from app/data/ASCII/cardxref.txt.
 */
class FixtureTest {

    @TempDir
    Path tmp;

    private static Path fixture(String name) throws URISyntaxException {
        return Path.of(FixtureTest.class.getResource("/fixtures/" + name).toURI());
    }

    private static List<String> golden() throws Exception {
        return Files.readAllLines(fixture("cbact03c-gnucobol-sysout.txt"), StandardCharsets.US_ASCII);
    }

    private static List<String> run(XrefFile file) {
        List<String> out = new ArrayList<>();
        new Cbact03c(file, out::add).run();
        return out;
    }

    @Test
    void asciiFixtureMatchesGnuCobolSysout() throws Exception {
        List<String> out = run(FixedWidthXrefFile.ascii(fixture("cardxref.txt")));
        assertEquals(102, out.size());
        assertEquals(golden(), out);
    }

    @Test
    void ebcdicFixtureMatchesGnuCobolSysout() throws Exception {
        assertEquals(golden(), run(FixedWidthXrefFile.ebcdic(fixture("cardxref.ebcdic"))));
    }

    @Test
    void recordsAreReadInKeyOrderLikeAKsds() throws Exception {
        List<String> lines = new ArrayList<>(Files.readAllLines(fixture("cardxref.txt")));
        java.util.Collections.reverse(lines);
        Path reversed = tmp.resolve("reversed.txt");
        Files.write(reversed, lines);
        assertEquals(golden(), run(FixedWidthXrefFile.ascii(reversed)));
    }

    @Test
    void duplicateKeyFailsOpenWithStatus22() throws Exception {
        Path dup = tmp.resolve("dup.txt");
        Files.writeString(dup, "050002445376574000000005000000000050\n050002445376574000000005000000000050\n");
        List<String> out = new ArrayList<>();
        assertThrows(AbendException.class, () -> new Cbact03c(FixedWidthXrefFile.ascii(dup), out::add).run());
        assertEquals("FILE STATUS IS: NNNN0022", out.get(2));
    }

    @Test
    void overlongLineReadsWithStatus04() throws Exception {
        Path bad = tmp.resolve("long.txt");
        Files.writeString(bad, "0500024453765740000000050000000000500000000000000000\n");
        List<String> out = new ArrayList<>();
        assertThrows(AbendException.class, () -> new Cbact03c(FixedWidthXrefFile.ascii(bad), out::add).run());
        assertEquals(List.of("START OF EXECUTION OF PROGRAM CBACT03C", "ERROR READING XREFFILE",
                "FILE STATUS IS: NNNN0004", "ABENDING PROGRAM"), out);
    }

    @Test
    void truncatedFixedRecordReadsWithStatus04() throws Exception {
        byte[] data = Files.readAllBytes(fixture("cardxref.ebcdic"));
        Path bad = tmp.resolve("short.ebcdic");
        Files.write(bad, java.util.Arrays.copyOf(data, 75));
        List<String> out = new ArrayList<>();
        assertThrows(AbendException.class, () -> new Cbact03c(FixedWidthXrefFile.ebcdic(bad), out::add).run());
        assertEquals("ERROR READING XREFFILE", out.get(3));
        assertEquals("FILE STATUS IS: NNNN0004", out.get(4));
    }

    @Test
    void readBeforeOpenAndCloseBeforeOpenReturnStatuses() throws Exception {
        FixedWidthXrefFile file = FixedWidthXrefFile.ascii(fixture("cardxref.txt"));
        assertEquals("47", file.readNext().status());
        assertEquals("42", file.close());
    }

    @Test
    void mainRunsJclStepAndReturnsZero() throws Exception {
        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        ByteArrayOutputStream berr = new ByteArrayOutputStream();
        int rc = Cbact03cMain.run(new String[0], fixture("cardxref.txt").toString(),
                new PrintStream(bout, true, StandardCharsets.US_ASCII), new PrintStream(berr, true, StandardCharsets.US_ASCII));
        assertEquals(0, rc);
        assertEquals(golden(), bout.toString(StandardCharsets.US_ASCII).lines().toList());
    }

    @Test
    void mainEbcdicFlag() throws Exception {
        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        int rc = Cbact03cMain.run(new String[] {fixture("cardxref.ebcdic").toString(), "--ebcdic"}, null,
                new PrintStream(bout, true, StandardCharsets.US_ASCII), System.err);
        assertEquals(0, rc);
        assertEquals(golden(), bout.toString(StandardCharsets.US_ASCII).lines().toList());
    }

    @Test
    void mainMissingFileAbendsWith999AndMatchesGnuCobolMessages() throws IOException {
        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        ByteArrayOutputStream berr = new ByteArrayOutputStream();
        int rc = Cbact03cMain.run(new String[] {tmp.resolve("nonexist.dat").toString()}, null,
                new PrintStream(bout, true, StandardCharsets.US_ASCII), new PrintStream(berr, true, StandardCharsets.US_ASCII));
        assertEquals(999, rc);
        assertEquals(List.of("START OF EXECUTION OF PROGRAM CBACT03C", "ERROR OPENING XREFFILE",
                "FILE STATUS IS: NNNN0035", "ABENDING PROGRAM"), bout.toString(StandardCharsets.US_ASCII).lines().toList());
        assertTrue(berr.toString(StandardCharsets.US_ASCII).contains("ABEND U0999"));
    }

    @Test
    void mainWithoutXreffileIsUsageError() {
        ByteArrayOutputStream berr = new ByteArrayOutputStream();
        int rc = Cbact03cMain.run(new String[0], null, System.out, new PrintStream(berr, true, Charset.defaultCharset()));
        assertEquals(16, rc);
        assertTrue(berr.toString().startsWith("usage:"));
    }
}
