package com.carddemo.programs.paudbunl.io;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class FixedWidthFileIoTest {

    @TempDir
    Path tmp;

    @Test
    void writesRecfmFbWithoutDelimitersAndReadsBack() throws Exception {
        Path f = tmp.resolve("out.dat");
        FixedWidthFileRecordWriter w = new FixedWidthFileRecordWriter(f, 3);
        assertEquals("00", w.open());
        assertEquals("00", w.write(new byte[] {1, 2, 3}));
        assertEquals("00", w.write(new byte[] {4, 5, 6}));
        assertEquals("00", w.close());
        assertEquals(6, Files.size(f));
        try (FixedWidthFileRecordReader r = new FixedWidthFileRecordReader(f, 3)) {
            assertArrayEquals(new byte[] {1, 2, 3}, r.read().orElseThrow());
            assertArrayEquals(new byte[] {4, 5, 6}, r.read().orElseThrow());
            assertTrue(r.read().isEmpty());
        }
    }

    @Test
    void statusCodesForErrorPaths() throws Exception {
        FixedWidthFileRecordWriter w = new FixedWidthFileRecordWriter(tmp.resolve("x.dat"), 3);
        assertEquals(FileStatus.WRITE_NOT_OPEN, w.write(new byte[3]));
        assertEquals(FileStatus.NOT_OPEN, w.close());
        w.open();
        assertEquals(FileStatus.RECORD_LENGTH_CONFLICT, w.write(new byte[2]));
        assertEquals(FileStatus.ALREADY_OPEN, w.open());
        w.close();
        Path dirAsFile = tmp.resolve("adir");
        Files.createDirectory(dirAsFile);
        FixedWidthFileRecordWriter bad = new FixedWidthFileRecordWriter(dirAsFile, 3);
        assertEquals(FileStatus.PERMANENT_IO_ERROR, bad.open());
        assertFalse(bad.isOpen());
    }

    @Test
    void openCloseOkMeansSpacesOr00() {
        assertTrue(FileStatus.isOk("00"));
        assertTrue(FileStatus.isOk("  "));
        assertFalse(FileStatus.isOk("30"));
    }
}
