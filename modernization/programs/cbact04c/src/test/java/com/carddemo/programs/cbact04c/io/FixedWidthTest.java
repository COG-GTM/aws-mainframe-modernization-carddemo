package com.carddemo.programs.cbact04c.io;

import com.carddemo.programs.cbact04c.copybook.CardXrefRecord;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFileAttributeView;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

class FixedWidthTest {

    @TempDir
    Path dir;

    private static final CardXrefRecord XREF = new CardXrefRecord("4111111111111111", 1, 2, "");

    private boolean posix() {
        return Files.getFileAttributeView(dir, PosixFileAttributeView.class) != null;
    }

    @Test
    void writeAllRewritesInPlaceKeepingPermissions() throws IOException {
        assumeTrue(posix());
        Path file = dir.resolve("acct.txt");
        Files.writeString(file, "old\n");
        Files.setPosixFilePermissions(file, PosixFilePermissions.fromString("rw-------"));
        Object inode = Files.readAttributes(file, "unix:ino").get("ino");

        FixedWidth.writeAll(file, CardXrefRecord.CODEC, List.of(XREF));

        assertEquals(List.of(XREF), FixedWidth.readAll(file, CardXrefRecord.CODEC));
        assertEquals("rw-------", PosixFilePermissions.toString(Files.getPosixFilePermissions(file)));
        assertEquals(inode, Files.readAttributes(file, "unix:ino").get("ino"));
        try (var files = Files.list(dir)) {
            assertEquals(1, files.count());
        }
    }

    @Test
    void writeAllUpdatesSymlinkTargetAndKeepsTheLink() throws IOException {
        Path target = dir.resolve("real.txt");
        Files.writeString(target, "old\n");
        Path link = Files.createSymbolicLink(dir.resolve("link.txt"), target);

        FixedWidth.writeAll(link, CardXrefRecord.CODEC, List.of(XREF));

        assertTrue(Files.isSymbolicLink(link));
        assertEquals(List.of(XREF), FixedWidth.readAll(target, CardXrefRecord.CODEC));
    }

    @Test
    void indexedFileOpenedIoWithoutRewriteLeavesFileUntouched() throws IOException {
        Path file = dir.resolve("xref.txt");
        byte[] original = (CardXrefRecord.CODEC.encode(XREF).stripTrailing() + "\r\n").getBytes(FixedWidth.CHARSET);
        Files.write(file, original);
        FixedWidthIndexedFile<Long, CardXrefRecord> f =
                new FixedWidthIndexedFile<>(file, CardXrefRecord.CODEC, CardXrefRecord::xrefAcctId);

        assertEquals(FileStatus.OK, f.open(OpenMode.I_O));
        assertEquals(FileStatus.OK, f.read(2L).status());
        assertEquals(FileStatus.OK, f.close());

        assertArrayEquals(original, Files.readAllBytes(file));
    }

    @Test
    void indexedFileOpenIoOnReadOnlyFileReturns37() throws IOException {
        assumeTrue(posix());
        Path file = dir.resolve("acct.txt");
        Files.writeString(file, CardXrefRecord.CODEC.encode(XREF) + "\n");
        Files.setPosixFilePermissions(file, PosixFilePermissions.fromString("r--r--r--"));
        assumeTrue(!Files.isWritable(file));
        FixedWidthIndexedFile<Long, CardXrefRecord> f =
                new FixedWidthIndexedFile<>(file, CardXrefRecord.CODEC, CardXrefRecord::xrefAcctId);

        assertEquals(FileStatus.OPEN_MODE_NOT_ALLOWED, f.open(OpenMode.I_O));
        assertEquals(FileStatus.OK, f.open(OpenMode.INPUT));
    }
}
