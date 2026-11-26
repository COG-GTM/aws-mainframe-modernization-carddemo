package com.carddemo.programs.cbstm03b.io;

import com.carddemo.programs.cbstm03b.copybook.FileStatus;
import org.junit.jupiter.api.Test;

import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class FixedWidthIoTest {

    @Test
    void readerPadsTrimmedLinesToLreclAndSkipsBlankLines() throws IOException {
        List<String> recs = new FixedWidthRecordReader(5).readAll(new StringReader("ab\n\nabcde\n"));
        assertEquals(List.of("ab   ", "abcde"), recs);
    }

    @Test
    void readerRejectsOverlongLine() {
        assertThrows(RecordLengthException.class, () -> new FixedWidthRecordReader(3).readAll(new StringReader("abcd\n")));
    }

    @Test
    void writerWritesOneRecordPerLineAndChecksLength() throws IOException {
        StringWriter w = new StringWriter();
        new FixedWidthRecordWriter(3).writeAll(w, List.of("abc", "d  "));
        assertEquals("abc\nd  \n", w.toString());
        assertThrows(IOException.class, () -> new FixedWidthRecordWriter(3).writeAll(new StringWriter(), List.of("ab")));
    }

    @Test
    void indexedFileRejectsDuplicateKeysWithoutExposingTheKey() {
        IllegalArgumentException dup = assertThrows(IllegalArgumentException.class,
                () -> InMemoryIndexedFile.of("F", 18, 0, 16, List.of("4000000000000001AA", "4000000000000001BB")));
        assertEquals("F: duplicate key at record 2", dup.getMessage());
        assertFalse(dup.getMessage().contains("4000000000000001"));
    }

    @Test
    void indexedFileRejectsBadLengths() {
        assertThrows(IllegalArgumentException.class,
                () -> InMemoryIndexedFile.of("F", 4, 0, 2, List.of("aa1")));
    }

    @Test
    void indexedFileKeyAtOffsetAndRandomRead() {
        InMemoryIndexedFile f = InMemoryIndexedFile.of("F", 4, 2, 2, List.of("xxBB", "yyAA"));
        assertEquals(FileStatus.SUCCESS, f.open());
        assertEquals("yyAA", f.readNext().record());
        assertEquals("xxBB", f.readByKey("BB").record());
        assertEquals(FileStatus.RECORD_NOT_FOUND, f.readByKey("CC").status());
        assertThrows(IllegalArgumentException.class, () -> f.readByKey("C"));
    }

    @Test
    void keysAreReadInEbcdicOrder() {
        InMemoryIndexedFile f = InMemoryIndexedFile.of("F", 2, 0, 2, List.of("11", "A1", "a1"));
        f.open();
        assertEquals("a1", f.readNext().record());
        assertEquals("A1", f.readNext().record());
        assertEquals("11", f.readNext().record());
        assertTrue(EbcdicCollation.compare("A", "1") < 0);
        assertTrue("A".compareTo("1") > 0, "ASCII order differs");
        InMemoryIndexedFile unmappable = InMemoryIndexedFile.of("F", 3, 0, 1, List.of("\u20acAA", "\u0100BB"));
        unmappable.open();
        assertEquals("\u20acAA", unmappable.readByKey("\u20ac").record(), "keys outside CP037 do not collide");
        assertEquals("\u0100BB", unmappable.readByKey("\u0100").record());
    }

    @Test
    void deferredFileReportsLoadProblemsAsOpenStatuses(@TempDir Path tmp) throws IOException {
        Path bad = tmp.resolve("bad.txt");
        Files.writeString(bad, "toolong\n");
        FixedWidthRecordReader reader = new FixedWidthRecordReader(4);
        assertEquals("39", InMemoryIndexedFile.deferred("F", 4, 0, 2, () -> reader.readAll(bad)).open().code(),
                "line longer than LRECL: wrong record length");
        assertEquals("30", InMemoryIndexedFile.deferred("F", 4, 0, 2, () -> reader.readAll(tmp)).open().code(),
                "directory: I/O error");
        assertEquals("35", InMemoryIndexedFile.deferred("F", 4, 0, 2,
                () -> reader.readAll(tmp.resolve("absent.txt"))).open().code());
        assertEquals("39", InMemoryIndexedFile.deferred("F", 4, 0, 2, () -> List.of("abc")).open().code(),
                "wrong record length");
        assertEquals("39", InMemoryIndexedFile.deferred("F", 4, 0, 2, () -> List.of("aa11", "aa22")).open().code(),
                "duplicate key");
        InMemoryIndexedFile ok = InMemoryIndexedFile.deferred("F", 4, 0, 2, () -> List.of("aa11"));
        assertEquals("00", ok.open().code());
        assertEquals("aa11", ok.readNext().record());
    }
}
