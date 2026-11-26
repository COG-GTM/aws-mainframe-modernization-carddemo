package com.carddemo.programs.cbtrn02c.io;

import org.junit.jupiter.api.Test;

import java.nio.file.Files;
import java.io.UncheckedIOException;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
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
    void savesFollowSymlinksKeepPermissionsAndCreateOwnerOnlyFiles() throws Exception {
        Path dir = Files.createTempDirectory("ksds");
        Path real = dir.resolve("real.txt");
        Files.writeString(real, "AAA1\n");
        Files.setPosixFilePermissions(real, PosixFilePermissions.fromString("rw-rw----"));
        Path link = Files.createSymbolicLink(dir.resolve("link.txt"), real);
        FileIndexedFile f = new FileIndexedFile(link, 6, 0, 3);
        assertEquals("00", f.open(OpenMode.I_O));
        f.write("BBB2");
        f.close();
        assertTrue(Files.isSymbolicLink(link));
        assertEquals(List.of("AAA1  ", "BBB2  "), Files.readAllLines(real));
        assertEquals("rw-rw----", PosixFilePermissions.toString(Files.getPosixFilePermissions(real)));

        Path created = dir.resolve("new.txt");
        LineFiles.writeRecords(created, List.of("CCC3"));
        assertEquals("rw-------", PosixFilePermissions.toString(Files.getPosixFilePermissions(created)));
        try (var files = Files.list(dir)) {
            assertEquals(3, files.count());
        }
    }

    @Test
    void savesThroughDanglingSymlinksAndToDevices() throws Exception {
        Path dir = Files.createTempDirectory("ksds");
        Path link = Files.createSymbolicLink(dir.resolve("out.txt"), Path.of("sub", "rejects.txt"));
        Files.createDirectory(dir.resolve("sub"));
        LineFiles.writeRecords(link, List.of("REJ1"));
        assertTrue(Files.isSymbolicLink(link));
        assertEquals(List.of("REJ1"), Files.readAllLines(dir.resolve("sub").resolve("rejects.txt")));

        Path devNull = Path.of("/dev/null");
        LineFiles.writeRecords(devNull, List.of("REJ2"));
        assertTrue(Files.exists(devNull) && !Files.isRegularFile(devNull));
    }

    @Test
    void failedSaveLeavesThePreviousFileIntact() throws Exception {
        Path dir = Files.createTempDirectory("ksds");
        Path p = dir.resolve("f.txt");
        Files.writeString(p, "AAA1\n");
        Files.setPosixFilePermissions(dir, PosixFilePermissions.fromString("r-x------"));
        try {
            assertThrows(UncheckedIOException.class, () -> LineFiles.writeRecords(p, List.of("BBB2")));
        } finally {
            Files.setPosixFilePermissions(dir, PosixFilePermissions.fromString("rwx------"));
        }
        assertEquals("AAA1\n", Files.readString(p));
    }

    @Test
    void fileBackedKsdsRefusesDuplicateKeysAndUnreadablePaths() throws Exception {
        Path dir = Files.createTempDirectory("ksds");
        Path p = dir.resolve("dup.txt");
        Files.writeString(p, "AAA1\nAAA2\n");
        assertEquals("30", new FileIndexedFile(p, 6, 0, 3).open(OpenMode.I_O));
        assertEquals("30", new FileIndexedFile(dir, 6, 0, 3).open(OpenMode.INPUT));
        assertEquals("30", new LineSequentialReader(dir, 6).open());
        Path blocked = dir.resolve("blocked.txt");
        Files.writeString(blocked, "AAA1AAA2AAA3\n");
        assertEquals("30", new LineSequentialReader(blocked, 6).open());
        assertEquals(List.of("AAA1", "AAA2"), Files.readAllLines(p));
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
