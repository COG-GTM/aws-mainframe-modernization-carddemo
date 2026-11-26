package com.carddemo.programs.cobtupdt;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.StringReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class FixedWidthTrRecordReaderTest {

    @Test
    void lineSequentialStripsCrLf(@TempDir Path dir) throws Exception {
        Path f = dir.resolve("inp.txt");
        Files.writeString(f, "A01One\r\nD02\r\n", StandardCharsets.UTF_8);
        FixedWidthTrRecordReader r = FixedWidthTrRecordReader.of(f);
        assertEquals("00", r.open());
        assertEquals(Optional.of("A01One"), r.readNext());
        assertEquals(Optional.of("D02"), r.readNext());
        assertEquals(Optional.empty(), r.readNext());
        assertEquals("00", r.close());
    }

    @Test
    void lineSequentialTruncatesOversizedLinesOnRead() {
        String longLine = "A01" + "x".repeat(1_000_000);
        FixedWidthTrRecordReader r = FixedWidthTrRecordReader.ofLines(List.of(longLine, "D02"));
        r.open();
        assertEquals(Optional.of(longLine.substring(0, 53)), r.readNext());
        assertEquals(Optional.of("D02"), r.readNext());
        assertEquals(Optional.empty(), r.readNext());
    }

    @Test
    void lineSequentialStripsCrOnExactly53CharLine() {
        String rec = TrRecord.parse("A01One").toFixedWidth();
        FixedWidthTrRecordReader r = new FixedWidthTrRecordReader(
                () -> new StringReader(rec + "\r\nU02"), FixedWidthTrRecordReader.Layout.LINE_SEQUENTIAL);
        r.open();
        assertEquals(Optional.of(rec), r.readNext());
        assertEquals(Optional.of("U02"), r.readNext());
        assertEquals(Optional.empty(), r.readNext());
    }

    @Test
    void lineSequentialAcceptsCrOnlyAndMixedTerminators() {
        FixedWidthTrRecordReader r = new FixedWidthTrRecordReader(
                () -> new StringReader("*Comment\rA01One\r\nD02\n\rU03"), FixedWidthTrRecordReader.Layout.LINE_SEQUENTIAL);
        r.open();
        assertEquals(Optional.of("*Comment"), r.readNext());
        assertEquals(Optional.of("A01One"), r.readNext());
        assertEquals(Optional.of("D02"), r.readNext());
        assertEquals(Optional.of(""), r.readNext());
        assertEquals(Optional.of("U03"), r.readNext());
        assertEquals(Optional.empty(), r.readNext());
    }

    @Test
    void closeFailureGivesStatus30() {
        FixedWidthTrRecordReader r = new FixedWidthTrRecordReader(() -> new java.io.FilterReader(new StringReader("")) {
            @Override
            public void close() throws java.io.IOException {
                throw new java.io.IOException("x");
            }
        }, FixedWidthTrRecordReader.Layout.LINE_SEQUENTIAL);
        r.open();
        assertEquals(Optional.empty(), r.readNext());
        CobolFileStatusException e = assertThrows(CobolFileStatusException.class, r::close);
        assertEquals("30", e.fileStatus());
    }

    @Test
    void fixedBlockReads53CharRecords() {
        String rec1 = TrRecord.parse("A01One").toFixedWidth();
        String rec2 = TrRecord.parse("U02Two").toFixedWidth();
        FixedWidthTrRecordReader r = new FixedWidthTrRecordReader(
                () -> new StringReader(rec1 + rec2), FixedWidthTrRecordReader.Layout.FIXED_BLOCK);
        r.open();
        assertEquals(Optional.of(rec1), r.readNext());
        assertEquals(Optional.of(rec2), r.readNext());
        assertEquals(Optional.empty(), r.readNext());
    }

    @Test
    void missingFileGivesStatus35AndReadGives47() {
        FixedWidthTrRecordReader r = FixedWidthTrRecordReader.of(Path.of("no/such/file"));
        assertEquals("35", r.open());
        CobolFileStatusException e = assertThrows(CobolFileStatusException.class, r::readNext);
        assertEquals("47", e.fileStatus());
        assertEquals("42", r.close());
    }

    @Test
    void doubleOpenGivesStatus41() {
        FixedWidthTrRecordReader r = FixedWidthTrRecordReader.ofLines(List.of("A01"));
        assertEquals("00", r.open());
        assertEquals("41", r.open());
    }
}
