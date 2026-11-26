package com.carddemo.programs.cbimport.io;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermissions;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class FixedWidthRecordFileWriterTest {

    @TempDir
    Path dir;

    @Test
    void createsOwnerOnlyFileAndWritesFixedBlockRecords() throws Exception {
        Path p = dir.resolve("out");
        var w = new FixedWidthRecordFileWriter(p, 3, Charset.forName("IBM037"), "");
        assertEquals("00", w.open());
        assertTrue(w.created());
        assertEquals("00", w.write("A1 "));
        assertEquals("00", w.close());
        assertArrayEquals(new byte[] {(byte) 0xC1, (byte) 0xF1, 0x40}, Files.readAllBytes(p));
        if (Files.getFileStore(p).supportsFileAttributeView("posix")) {
            assertEquals("rw-------", PosixFilePermissions.toString(Files.getPosixFilePermissions(p)));
        }
    }

    @Test
    void existingDatasetFailsOpenAndIsLeftUntouched() throws Exception {
        Path p = Files.writeString(dir.resolve("out"), "previous import");
        var w = new FixedWidthRecordFileWriter(p, 3, StandardCharsets.US_ASCII, "\n");
        assertEquals("30", w.open());
        assertFalse(w.created());
        assertEquals("previous import", Files.readString(p));
    }

    @Test
    void symlinkAtDestinationFailsOpenWithoutTouchingTarget() throws Exception {
        Path target = Files.writeString(dir.resolve("target"), "keep");
        Path link = Files.createSymbolicLink(dir.resolve("link"), target);
        var w = new FixedWidthRecordFileWriter(link, 3, StandardCharsets.US_ASCII, "\n");
        assertEquals("30", w.open());
        assertEquals("keep", Files.readString(target));
    }

    @Test
    void missingDirectoryIsStatus35() {
        var w = new FixedWidthRecordFileWriter(dir.resolve("no/such/out"), 3, StandardCharsets.US_ASCII, "");
        assertEquals("35", w.open());
    }

    @Test
    void wrongLengthOrUnencodableRecordIsRejected() throws Exception {
        Path p = dir.resolve("out");
        var w = new FixedWidthRecordFileWriter(p, 3, StandardCharsets.US_ASCII, "");
        w.open();
        assertEquals("44", w.write("ABCD"));
        assertEquals("30", w.write("A\u00e9B"));
        w.close();
        assertEquals(0, Files.size(p));
    }

    @Test
    void multiByteEncodingThatChangesRecordLengthIsRejected() throws Exception {
        var w = new FixedWidthRecordFileWriter(dir.resolve("out"), 3, StandardCharsets.UTF_8, "");
        w.open();
        assertEquals("44", w.write("A\u00e9B"));
        w.close();
    }
}
