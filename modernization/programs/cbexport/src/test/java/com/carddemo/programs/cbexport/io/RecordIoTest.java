package com.carddemo.programs.cbexport.io;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;

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
    void writerOpenFailureReturnsStatus30() {
        assertEquals(FileStatus.PERMANENT_ERROR,
                new FixedLengthRecordWriter(dir.resolve("missing/dir/out.dat"), 3).open());
    }
}
