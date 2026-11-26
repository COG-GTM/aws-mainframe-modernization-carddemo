package com.carddemo.programs.cbtrn01c.io;

import com.carddemo.programs.cbtrn01c.copybook.CardXrefRecord;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

class FixedWidthFileTest {

    @TempDir
    Path tmp;

    @Test
    void sequentialReadsPadsShortLinesAndSignalsEof() throws Exception {
        Path f = Files.writeString(tmp.resolve("x.txt"), "123456789012345600000000100000000002\n");
        FixedWidthSequentialFile<CardXrefRecord> file = new FixedWidthSequentialFile<>(f, CardXrefRecord.CODEC);
        assertEquals("47", file.read().status());
        assertEquals("00", file.open());
        ReadResult<CardXrefRecord> r = file.read();
        assertEquals("00", r.status());
        assertEquals(2, r.record().acctId());
        assertEquals("10", file.read().status());
        assertEquals("00", file.close());
        assertEquals("42", file.close());
    }

    @Test
    void sequentialOpenMissingFileIs35() {
        assertEquals("35", new FixedWidthSequentialFile<>(tmp.resolve("none"), CardXrefRecord.CODEC).open());
    }

    @Test
    void indexedRandomReadByKey() throws Exception {
        Path f = Files.writeString(tmp.resolve("x.txt"),
                "111111111111111100000000100000000002\n222222222222222200000000300000000004\n");
        FixedWidthIndexedFile<CardXrefRecord> file = new FixedWidthIndexedFile<>(f, CardXrefRecord.CODEC, 16);
        assertEquals("47", file.read("1111111111111111").status());
        assertEquals("00", file.open());
        assertEquals(4, file.read("2222222222222222").record().acctId());
        ReadResult<CardXrefRecord> miss = file.read("3333333333333333");
        assertEquals("23", miss.status());
        assertNull(miss.record());
        assertEquals("00", file.close());
        assertEquals("42", file.close());
        assertEquals("35", new FixedWidthIndexedFile<>(tmp.resolve("none"), CardXrefRecord.CODEC, 16).open());
    }

    @Test
    void indexedOverlongRecordFailsOpen() throws Exception {
        Path f = Files.writeString(tmp.resolve("x.txt"), "X".repeat(51) + "\n");
        assertEquals("04", new FixedWidthIndexedFile<>(f, CardXrefRecord.CODEC, 16).open());
    }

    @Test
    void fileStatusPredicates() {
        assertEquals(true, FileStatus.isSuccessful("02"));
        assertEquals(false, FileStatus.isSuccessful("10"));
        assertEquals(true, FileStatus.isInvalidKey("23"));
        assertEquals(false, FileStatus.isInvalidKey("35"));
    }
}
