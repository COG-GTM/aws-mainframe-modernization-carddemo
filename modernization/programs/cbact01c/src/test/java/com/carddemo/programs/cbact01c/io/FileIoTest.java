package com.carddemo.programs.cbact01c.io;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;

class FileIoTest {
    @TempDir
    Path dir;

    @Test
    void ksdsReaderMissingFileIsStatus35() {
        assertEquals("35", new KsdsFileReader(dir.resolve("none"), 300, 11).open());
    }

    @Test
    void ksdsReaderReturnsKeyOrderPadsShortLinesThenEof() throws Exception {
        Path f = dir.resolve("acct.txt");
        Files.writeString(f, "00000000002B\n00000000001A\n", StandardCharsets.ISO_8859_1);
        KsdsFileReader r = new KsdsFileReader(f, 20, 11);
        assertEquals("00", r.open());
        ReadResult first = r.read();
        assertEquals("00", first.status());
        assertEquals("00000000001A        ", new String(first.record(), StandardCharsets.ISO_8859_1));
        assertEquals("00000000002B        ", new String(r.read().record(), StandardCharsets.ISO_8859_1));
        assertEquals("10", r.read().status());
        assertEquals("46", r.read().status());
        assertEquals("00", r.close());
        assertEquals("42", r.close());
    }

    @Test
    void ksdsReaderOverlongRecordIsStatus04() throws Exception {
        Path f = dir.resolve("acct.txt");
        Files.writeString(f, "X".repeat(21) + "\n", StandardCharsets.ISO_8859_1);
        KsdsFileReader r = new KsdsFileReader(f, 20, 11);
        r.open();
        assertEquals("04", r.read().status());
    }

    @Test
    void ksdsReaderNotOpenIsStatus47() {
        assertEquals("47", new KsdsFileReader(dir.resolve("x"), 20, 11).read().status());
    }

    @Test
    void fixedWriterWritesRecordsBackToBack() throws Exception {
        Path f = dir.resolve("fb.dat");
        FixedRecordFileWriter w = new FixedRecordFileWriter(f, 3);
        assertEquals("48", w.write(new byte[3]));
        assertEquals("00", w.open());
        assertEquals("00", w.write(new byte[] {1, 2, 3}));
        assertEquals("44", w.write(new byte[] {1, 2}));
        assertEquals("00", w.write(new byte[] {4, 5, 6}));
        assertEquals("00", w.close());
        assertArrayEquals(new byte[] {1, 2, 3, 4, 5, 6}, Files.readAllBytes(f));
    }

    @Test
    void fixedWriterOpenFailureIsStatus30() {
        assertEquals("30", new FixedRecordFileWriter(dir.resolve("no/such/dir/f"), 3).open());
    }

    @Test
    void variableWriterIbmRdwIncludesHeaderLength() throws Exception {
        Path f = dir.resolve("vb.dat");
        VariableRecordFileWriter w = new VariableRecordFileWriter(f, 10, 80,
                VariableRecordFileWriter.HeaderFormat.IBM_RDW);
        w.open();
        assertEquals("00", w.write("ABCDEFGHIJKL".getBytes(StandardCharsets.ISO_8859_1)));
        assertEquals("44", w.write(new byte[9]));
        assertEquals("44", w.write(new byte[81]));
        w.close();
        byte[] bytes = Files.readAllBytes(f);
        assertEquals(16, bytes.length);
        assertArrayEquals(new byte[] {0, 16, 0, 0}, java.util.Arrays.copyOf(bytes, 4));
    }

    @Test
    void variableWriterGnuCobolFormatCountsDataOnly() throws Exception {
        Path f = dir.resolve("vb.dat");
        VariableRecordFileWriter w = new VariableRecordFileWriter(f, 10, 80,
                VariableRecordFileWriter.HeaderFormat.GNUCOBOL_VARSEQ_0);
        w.open();
        w.write(new byte[39]);
        w.close();
        assertArrayEquals(new byte[] {0, 39, 0, 0}, java.util.Arrays.copyOf(Files.readAllBytes(f), 4));
    }
}
