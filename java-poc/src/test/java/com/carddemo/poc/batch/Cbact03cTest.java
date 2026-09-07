package com.carddemo.poc.batch;

import com.carddemo.poc.SampleData;
import com.carddemo.poc.io.FixedLengthRecordReader;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class Cbact03cTest {

    private static List<String> run(Path xrefFile) {
        ByteArrayOutputStream buf = new ByteArrayOutputStream();
        new Cbact03c(xrefFile, new PrintStream(buf, true, StandardCharsets.ISO_8859_1)).run();
        return buf.toString(StandardCharsets.ISO_8859_1).lines().toList();
    }

    /**
     * Expected SYSOUT, derived independently from the ASCII sample file: the COBOL program
     * displays every record twice (once in 1000-XREFFILE-GET-NEXT, once in the main loop).
     */
    private static List<String> expectedSysout() throws IOException {
        List<String> expected = new ArrayList<>();
        expected.add("START OF EXECUTION OF PROGRAM CBACT03C");
        for (String line : Files.readAllLines(SampleData.ascii("cardxref.txt"), StandardCharsets.ISO_8859_1)) {
            String record = String.format("%-50s", line);
            expected.add(record);
            expected.add(record);
        }
        expected.add("END OF EXECUTION OF PROGRAM CBACT03C");
        return expected;
    }

    @Test
    void reproducesCobolDisplayOutputFromEbcdicKsdsSource() throws IOException {
        List<String> out = run(SampleData.ebcdic("AWS.M2.CARDDEMO.CARDXREF.PS"));
        assertEquals(expectedSysout(), out);
        assertEquals(1 + 50 * 2 + 1, out.size());
    }

    @Test
    void reproducesCobolDisplayOutputFromAsciiSource() throws IOException {
        assertEquals(expectedSysout(), run(SampleData.ascii("cardxref.txt")));
    }

    @Test
    void firstAndLastRecordsMatchSampleData() {
        List<String> out = run(SampleData.ebcdic("AWS.M2.CARDDEMO.CARDXREF.PS"));
        assertEquals("0500024453765740" + "000000050" + "00000000050" + " ".repeat(14), out.get(1));
        assertEquals("9805583408996588" + "000000040" + "00000000040" + " ".repeat(14), out.get(out.size() - 2));
    }

    @Test
    void abendsWithFileStatus35WhenXreffileMissing() {
        ByteArrayOutputStream buf = new ByteArrayOutputStream();
        Cbact03c program = new Cbact03c(Path.of("missing.PS"), new PrintStream(buf, true, StandardCharsets.ISO_8859_1));

        AbendException abend = assertThrows(AbendException.class, program::run);

        assertEquals(999, abend.abendCode());
        List<String> out = buf.toString(StandardCharsets.ISO_8859_1).lines().toList();
        assertEquals(List.of(
                "START OF EXECUTION OF PROGRAM CBACT03C",
                "ERROR OPENING XREFFILE",
                "FILE STATUS IS: NNNN0035",
                "ABENDING PROGRAM"), out);
        assertTrue(out.stream().noneMatch(l -> l.startsWith("END OF EXECUTION")));
    }

    @Test
    void closesXreffileWhenReadAbends(@TempDir Path dir) throws IOException {
        Path truncated = dir.resolve("short.PS");
        byte[] full = Files.readAllBytes(SampleData.ebcdic("AWS.M2.CARDDEMO.CARDXREF.PS"));
        Files.write(truncated, java.util.Arrays.copyOf(full, 50 + 10));
        FixedLengthRecordReader reader = FixedLengthRecordReader.forDataFile(truncated, 50);
        ByteArrayOutputStream buf = new ByteArrayOutputStream();
        Cbact03c program = new Cbact03c(reader, new PrintStream(buf, true, StandardCharsets.ISO_8859_1));

        AbendException abend = assertThrows(AbendException.class, program::run);

        assertEquals("READ XREFFILE status 30", abend.getMessage());
        assertFalse(reader.isOpen());
        assertTrue(buf.toString(StandardCharsets.ISO_8859_1).contains("ERROR READING XREFFILE"));
    }
}
