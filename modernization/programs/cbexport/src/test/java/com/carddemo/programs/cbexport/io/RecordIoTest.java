package com.carddemo.programs.cbexport.io;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class RecordIoTest {

    @TempDir
    Path dir;

    @Test
    void lineSequentialPadsShortLinesAndFlagsLongOnes() throws Exception {
        Path f = dir.resolve("in.txt");
        Files.writeString(f, "AB\nABCDEF\n");
        LineSequentialRecordReader r = new LineSequentialRecordReader(f, 4);
        assertEquals(FileStatus.OK, r.open());
        ReadResult first = r.read();
        assertEquals(FileStatus.OK, first.status());
        assertEquals("AB  ", new String(first.record()));
        assertEquals(FileStatus.LENGTH_MISMATCH, r.read().status());
        assertEquals(FileStatus.EOF, r.read().status());
        r.close();
    }

    @Test
    void lineSequentialHandlesCrlfAndBoundsOversizedLines() throws Exception {
        Path f = dir.resolve("crlf.txt");
        Files.writeString(f, "ABCD\r\n" + "X".repeat(100_000) + "\nEF");
        LineSequentialRecordReader r = new LineSequentialRecordReader(f, 4);
        assertEquals(FileStatus.OK, r.open());
        assertEquals("ABCD", new String(r.read().record()));
        assertEquals(FileStatus.LENGTH_MISMATCH, r.read().status());
        assertEquals("EF  ", new String(r.read().record()));
        assertEquals(FileStatus.EOF, r.read().status());
        r.close();
    }

    @Test
    void missingInputReturnsStatus35() {
        assertEquals(FileStatus.FILE_NOT_FOUND, new LineSequentialRecordReader(dir.resolve("nope"), 4).open());
        assertEquals(FileStatus.FILE_NOT_FOUND, new FixedLengthRecordReader(dir.resolve("nope"), 4).open());
    }

    @Test
    void fixedLengthReaderReadsBlocksAndFlagsPartialTail() throws Exception {
        Path f = dir.resolve("in.dat");
        Files.write(f, new byte[] {1, 2, 3, 4, 5, 6, 7});
        FixedLengthRecordReader r = new FixedLengthRecordReader(f, 3);
        assertEquals(FileStatus.OK, r.open());
        assertArrayEquals(new byte[] {1, 2, 3}, r.read().record());
        assertArrayEquals(new byte[] {4, 5, 6}, r.read().record());
        assertEquals(FileStatus.LENGTH_MISMATCH, r.read().status());
        r.close();
    }

    @Test
    void fixedLengthWriterEnforcesLengthAndAscendingKey() throws Exception {
        Path f = dir.resolve("out.dat");
        FixedLengthRecordWriter w = new FixedLengthRecordWriter(f, 3, 1, 1);
        assertEquals(FileStatus.OK, w.open());
        assertEquals(FileStatus.OK, w.write(new byte[] {9, 1, 9}));
        assertEquals(FileStatus.OK, w.write(new byte[] {9, (byte) 0x80, 9}));
        assertEquals(FileStatus.SEQUENCE_ERROR, w.write(new byte[] {9, (byte) 0x80, 9}));
        assertEquals(FileStatus.SEQUENCE_ERROR, w.write(new byte[] {9, 2, 9}));
        assertEquals(FileStatus.RECORD_LENGTH_ERROR, w.write(new byte[] {1}));
        w.close();
        assertArrayEquals(new byte[] {9, 1, 9, 9, (byte) 0x80, 9}, Files.readAllBytes(f));
    }

    @Test
    void writerCreatesOwnerOnlyFile() throws Exception {
        Path f = dir.resolve("perm.dat");
        org.junit.jupiter.api.Assumptions.assumeTrue(
                Files.getFileStore(dir).supportsFileAttributeView("posix"));
        Files.writeString(f, "old");
        Files.setPosixFilePermissions(f, java.nio.file.attribute.PosixFilePermissions.fromString("rw-r--r--"));
        FixedLengthRecordWriter w = new FixedLengthRecordWriter(f, 1);
        assertEquals(FileStatus.OK, w.open());
        assertEquals("old", Files.readString(f), "target untouched until CLOSE");
        assertEquals(FileStatus.OK, w.write(new byte[] {'n'}));
        w.close();
        w.close();
        assertEquals("rw-------", java.nio.file.attribute.PosixFilePermissions.toString(
                Files.getPosixFilePermissions(f)));
        assertEquals("n", Files.readString(f));
        try (var s = Files.list(dir)) {
            assertEquals(1, s.count(), "no temp files left behind");
        }
    }

    @Test
    void writerReplacesSymlinkInsteadOfFollowingIt() throws Exception {
        Path victim = dir.resolve("victim.txt");
        Files.writeString(victim, "precious");
        Path link = dir.resolve("link.dat");
        try {
            Files.createSymbolicLink(link, victim);
        } catch (UnsupportedOperationException | java.io.IOException e) {
            org.junit.jupiter.api.Assumptions.abort("symlinks not supported: " + e);
        }
        FixedLengthRecordWriter w = new FixedLengthRecordWriter(link, 1);
        assertEquals(FileStatus.OK, w.open());
        w.close();
        assertEquals("precious", Files.readString(victim));
        assertEquals(false, Files.isSymbolicLink(link));
    }

    @Test
    void writerReplacesSymlinkToDirectory() throws Exception {
        Path target = Files.createDirectory(dir.resolve("target"));
        Path link = dir.resolve("link");
        try {
            Files.createSymbolicLink(link, target);
        } catch (UnsupportedOperationException | java.io.IOException e) {
            org.junit.jupiter.api.Assumptions.abort("symlinks not supported: " + e);
        }
        FixedLengthRecordWriter w = new FixedLengthRecordWriter(link, 3);
        assertEquals(FileStatus.OK, w.open());
        assertEquals(FileStatus.OK, w.write(new byte[] {1, 2, 3}));
        w.close();
        assertFalse(w.publishFailed());
        assertTrue(Files.isRegularFile(link, java.nio.file.LinkOption.NOFOLLOW_LINKS));
        assertTrue(Files.isDirectory(target));
    }

    @Test
    void writerReportsPublishFailure() throws Exception {
        Path target = dir.resolve("exp.dat");
        FixedLengthRecordWriter w = new FixedLengthRecordWriter(target, 3);
        assertEquals(FileStatus.OK, w.open());
        Files.createDirectory(target);
        Files.writeString(target.resolve("child"), "x");
        w.close();
        assertTrue(w.publishFailed());
        assertTrue(Files.isDirectory(target));
    }

    @Test
    void writerRejectsDirectoryTarget() {
        assertEquals(FileStatus.PERMANENT_ERROR, new FixedLengthRecordWriter(dir, 3).open());
    }

    @Test
    void writerOpenFailureReturnsStatus30() {
        assertEquals(FileStatus.PERMANENT_ERROR,
                new FixedLengthRecordWriter(dir.resolve("missing/dir/out.dat"), 3).open());
    }
}
