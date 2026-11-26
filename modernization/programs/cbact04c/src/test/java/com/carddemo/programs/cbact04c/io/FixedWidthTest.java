package com.carddemo.programs.cbact04c.io;

import com.carddemo.programs.cbact04c.copybook.CardXrefRecord;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class FixedWidthTest {

    @TempDir
    Path dir;

    private static final CardXrefRecord XREF = new CardXrefRecord("4111111111111111", 1, 2, "");

    @Test
    void writeAllReplacesContentKeepingPermissionsAndLeavesSiblingsAlone() throws IOException {
        Path file = dir.resolve("acct.txt");
        Files.writeString(file, "old\n");
        Files.setPosixFilePermissions(file, PosixFilePermissions.fromString("rw-------"));
        Path sibling = dir.resolve("acct.txt.tmp");
        Files.writeString(sibling, "keep");

        FixedWidth.writeAll(file, CardXrefRecord.CODEC, List.of(XREF));

        assertEquals(List.of(XREF), FixedWidth.readAll(file, CardXrefRecord.CODEC));
        assertEquals("rw-------", PosixFilePermissions.toString(Files.getPosixFilePermissions(file)));
        assertEquals("keep", Files.readString(sibling));
        try (var files = Files.list(dir)) {
            assertEquals(2, files.count());
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
}
