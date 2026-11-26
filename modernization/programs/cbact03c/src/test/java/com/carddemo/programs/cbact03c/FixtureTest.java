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

    @Test
    void ebcdicKeysCollateOnRawBytesLettersBeforeDigits() throws Exception {
        String digitKey = "1000000000000000000000001000000000001             ";
        String letterKey = "A000000000000000000000002000000000002             ";
        Path f = tmp.resolve("mixed.ebcdic");
        Charset ibm037 = Charset.forName("IBM037");
        Files.write(f, (digitKey + letterKey).getBytes(ibm037));
        assertEquals(List.of("START OF EXECUTION OF PROGRAM CBACT03C", letterKey, letterKey, digitKey, digitKey,
                "END OF EXECUTION OF PROGRAM CBACT03C"), run(FixedWidthXrefFile.ebcdic(f)));

        Path a = tmp.resolve("mixed.txt");
        Files.writeString(a, letterKey + "\n" + digitKey + "\n");
        assertEquals(List.of("START OF EXECUTION OF PROGRAM CBACT03C", digitKey, digitKey, letterKey, letterKey,
                "END OF EXECUTION OF PROGRAM CBACT03C"), run(FixedWidthXrefFile.ascii(a)));
    }

    @Test
    void truncatedTailIsReadAfterAllCompleteRecords() throws Exception {
        byte[] data = Files.readAllBytes(fixture("cardxref.ebcdic"));
        Path bad = tmp.resolve("tail.ebcdic");
        byte[] withTail = java.util.Arrays.copyOf(data, data.length + 3);
        System.arraycopy("000".getBytes(Charset.forName("IBM037")), 0, withTail, data.length, 3);
        Files.write(bad, withTail);
        List<String> out = new ArrayList<>();
        assertThrows(AbendException.class, () -> new Cbact03c(FixedWidthXrefFile.ebcdic(bad), out::add).run());
        List<String> expected = new ArrayList<>(golden().subList(0, 101));
        expected.addAll(List.of("ERROR READING XREFFILE", "FILE STATUS IS: NNNN0004", "ABENDING PROGRAM"));
        assertEquals(expected, out);
    }

    @Test
    void overlongLineSharingAKeyIsStatus04NotDuplicateKey() throws Exception {
        Path bad = tmp.resolve("longdup.txt");
        Files.writeString(bad, "050002445376574000000005000000000050\n"
                + "0500024453765740000000050000000000500000000000000000\n");
        List<String> out = new ArrayList<>();
        assertThrows(AbendException.class, () -> new Cbact03c(FixedWidthXrefFile.ascii(bad), out::add).run());
        assertEquals("FILE STATUS IS: NNNN0004", out.get(4));
    }

    @Test
    void mainReportsSysoutWriteFailure() throws Exception {
        PrintStream failing = new PrintStream(new java.io.OutputStream() {
            @Override
            public void write(int b) throws IOException {
                throw new IOException("No space left on device");
            }
        }, true, StandardCharsets.US_ASCII);
        ByteArrayOutputStream berr = new ByteArrayOutputStream();
        int rc = Cbact03cMain.run(new String[] {fixture("cardxref.txt").toString()}, null, failing,
                new PrintStream(berr, true, StandardCharsets.US_ASCII));
        assertEquals(12, rc);
        assertTrue(berr.toString(StandardCharsets.US_ASCII).contains("ERROR WRITING SYSOUT"));
    }

    @Test
    void ebcdicControlByteKeysKeepRawByteOrder() throws Exception {
        byte[] rec1 = "1000000000000000000000001000000000001             ".getBytes(Charset.forName("IBM037"));
        byte[] rec2 = rec1.clone();
        rec1[1] = 0x25;
        rec2[1] = 0x16;
        Path f = tmp.resolve("ctl.ebcdic");
        byte[] both = new byte[100];
        System.arraycopy(rec1, 0, both, 0, 50);
        System.arraycopy(rec2, 0, both, 50, 50);
        Files.write(f, both);
        FixedWidthXrefFile file = FixedWidthXrefFile.ebcdic(f);
        assertEquals("00", file.open());
        String first = file.readNext().record();
        String second = file.readNext().record();
        assertEquals(new String(rec2, Charset.forName("IBM037")), first);
        assertEquals(new String(rec1, Charset.forName("IBM037")), second);
    }

    @Test
    void fixedAsciiFormatReadsUndelimitedRecords() throws Exception {
        StringBuilder sb = new StringBuilder();
        for (String line : Files.readAllLines(fixture("cardxref.txt"))) {
            sb.append(line).append(" ".repeat(50 - line.length()));
        }
        Path f = tmp.resolve("cardxref.fixed");
        Files.writeString(f, sb, StandardCharsets.US_ASCII);
        assertEquals(golden(), run(FixedWidthXrefFile.fixedAscii(f)));

        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        int rc = Cbact03cMain.run(new String[] {f.toString(), "--fixed"}, null,
                new PrintStream(bout, true, StandardCharsets.US_ASCII), System.err);
        assertEquals(0, rc);
        assertEquals(golden(), bout.toString(StandardCharsets.US_ASCII).lines().toList());
    }

    @Test
    void mainReportsSysoutWriteFailureOnAbendToo() {
        PrintStream failing = new PrintStream(new java.io.OutputStream() {
            @Override
            public void write(int b) throws IOException {
                throw new IOException("No space left on device");
            }
        }, true, StandardCharsets.US_ASCII);
        ByteArrayOutputStream berr = new ByteArrayOutputStream();
        int rc = Cbact03cMain.run(new String[] {tmp.resolve("missing.dat").toString()}, null, failing,
                new PrintStream(berr, true, StandardCharsets.US_ASCII));
        assertEquals(999, rc);
        String err = berr.toString(StandardCharsets.US_ASCII);
        assertTrue(err.contains("ERROR WRITING SYSOUT"));
        assertTrue(err.contains("ABEND U0999"));
    }

    @Test
    void diagnosticDigitsIgnoreDefaultLocale() {
        java.util.Locale saved = java.util.Locale.getDefault(java.util.Locale.Category.FORMAT);
        try {
            java.util.Locale.setDefault(java.util.Locale.Category.FORMAT,
                    java.util.Locale.forLanguageTag("ar-EG-u-nu-arab"));
            assertEquals("ABEND U0999", new AbendException(999, 0).getMessage());
            assertEquals("9035", Cbact03c.formatIoStatus04("9#", StandardCharsets.ISO_8859_1));
        } finally {
            java.util.Locale.setDefault(java.util.Locale.Category.FORMAT, saved);
        }
    }
}
