package com.carddemo.programs.cbtrn02c.io;

import org.junit.jupiter.api.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class InMemoryIndexedFileTest {

    private final InMemoryIndexedFile file = new InMemoryIndexedFile(10, 0, 3);

    @Test
    void randomReadWriteRewriteStatuses() {
        file.load(List.of("AAA1111111", "BBB2"));
        assertEquals("47", file.read("AAA").status());
        assertEquals("00", file.open(OpenMode.I_O));
        assertEquals("41", file.open(OpenMode.I_O));
        assertEquals("AAA1111111", file.read("AAA").record());
        assertEquals("BBB2      ", file.read("BBB").record());
        assertEquals("23", file.read("CCC").status());
        assertEquals("22", file.write("AAA9"));
        assertEquals("00", file.write("CCC3"));
        assertEquals("00", file.rewrite("AAA0"));
        assertEquals("23", file.rewrite("DDD0"));
        assertEquals("00", file.close());
        assertEquals("42", file.close());
        assertEquals(List.of("AAA0      ", "BBB2      ", "CCC3      "), file.records());
    }

    @Test
    void openOutputEmptiesTheFileAndForbidsReadAndRewrite() {
        file.load(List.of("AAA1"));
        file.open(OpenMode.OUTPUT);
        assertEquals("47", file.read("AAA").status());
        assertEquals("49", file.rewrite("AAA1"));
        assertEquals("00", file.write("ZZZ1"));
        assertEquals(1, file.records().size());
    }

    @Test
    void inputModeForbidsWrite() {
        file.open(OpenMode.INPUT);
        assertEquals("48", file.write("AAA1"));
    }

    @Test
    void fileBackedKsdsLoadsPadsCrLfAndSavesOnClose() throws Exception {
        Path dir = Files.createTempDirectory("ksds");
        Path p = dir.resolve("f.txt");
        Files.writeString(p, "BBB2\r\nAAA1\n");
        FileIndexedFile f = new FileIndexedFile(p, 6, 0, 3);
        assertEquals("00", f.open(OpenMode.I_O));
        assertEquals("AAA1  ", f.read("AAA").record());
        f.write("CCC3");
        f.close();
        assertEquals(List.of("AAA1  ", "BBB2  ", "CCC3  "), Files.readAllLines(p));
        assertEquals("35", new FileIndexedFile(dir.resolve("missing"), 6, 0, 3).open(OpenMode.INPUT));
        assertTrue(Files.exists(p));
    }

    @Test
    void sequentialReaderAndWriterStatuses() throws Exception {
        LineSequentialReader r = new LineSequentialReader(List.of("AB", "CD"), 4);
        assertEquals("47", r.read().status());
        assertEquals("00", r.open());
        assertEquals("AB  ", r.read().record());
        assertEquals("CD  ", r.read().record());
        assertEquals("10", r.read().status());
        assertEquals("00", r.close());
        assertEquals("35", new LineSequentialReader(Path.of("does-not-exist"), 4).open());

        Path out = Files.createTempFile("seq", ".txt");
        LineSequentialWriter w = new LineSequentialWriter(out, 4);
        assertEquals("48", w.write("X"));
        w.open();
        w.write("X");
        w.close();
        assertEquals(List.of("X   "), Files.readAllLines(out));
    }
}
