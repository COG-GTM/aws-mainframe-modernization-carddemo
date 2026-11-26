package com.carddemo.programs.cbstm03b.io;

import com.carddemo.programs.cbstm03b.copybook.FileStatus;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class FixedWidthIoTest {

    @Test
    void readerPadsTrimmedLinesToLreclAndSkipsBlankLines() throws IOException {
        List<String> recs = new FixedWidthRecordReader(5).readAll(new StringReader("ab\n\nabcde\n"));
        assertEquals(List.of("ab   ", "abcde"), recs);
    }

    @Test
    void readerRejectsOverlongLine() {
        assertThrows(IOException.class, () -> new FixedWidthRecordReader(3).readAll(new StringReader("abcd\n")));
    }

    @Test
    void writerWritesOneRecordPerLineAndChecksLength() throws IOException {
        StringWriter w = new StringWriter();
        new FixedWidthRecordWriter(3).writeAll(w, List.of("abc", "d  "));
        assertEquals("abc\nd  \n", w.toString());
        assertThrows(IOException.class, () -> new FixedWidthRecordWriter(3).writeAll(new StringWriter(), List.of("ab")));
    }

    @Test
    void indexedFileRejectsDuplicateKeysAndBadLengths() {
        assertThrows(IllegalArgumentException.class,
                () -> InMemoryIndexedFile.of("F", 4, 0, 2, List.of("aa11", "aa22")));
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
}
