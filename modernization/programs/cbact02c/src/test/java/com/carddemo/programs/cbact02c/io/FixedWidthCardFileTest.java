package com.carddemo.programs.cbact02c.io;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

class FixedWidthCardFileTest {

    private static String rec(String key) {
        return key + "X".repeat(150 - key.length());
    }

    @Test
    void missingFileGivesStatus35(@TempDir Path dir) {
        assertEquals(FileStatus.FILE_NOT_FOUND, new FixedWidthCardFile(dir.resolve("nope")).open());
    }

    @Test
    void unreadablePathGivesStatus30(@TempDir Path dir) {
        assertEquals(FileStatus.PERMANENT_ERROR, new FixedWidthCardFile(dir).open());
    }

    @Test
    void fixedBlockSplitsEvery150BytesAndFlagsShortTail(@TempDir Path dir) throws IOException {
        Path file = dir.resolve("card.ps");
        String a = rec("1111111111111111");
        String b = rec("2222222222222222");
        Files.write(file, (b + a + "TAIL").getBytes(FixedWidthCardFile.EBCDIC));
        FixedWidthCardFile cardFile = FixedWidthCardFile.fixedBlockEbcdic(file);

        assertEquals("00", cardFile.open());
        assertEquals(a, cardFile.read().record());
        assertEquals(b, cardFile.read().record());
        CardFile.ReadResult tail = cardFile.read();
        assertEquals("04", tail.status());
        assertEquals("TAIL", tail.record());
    }

    @Test
    void readsRecordsInKeyOrderThenEof(@TempDir Path dir) throws IOException {
        Path file = dir.resolve("card.txt");
        Files.write(file, List.of(rec("2222222222222222"), rec("1111111111111111")));
        FixedWidthCardFile cardFile = new FixedWidthCardFile(file);

        assertEquals("00", cardFile.open());
        assertEquals(rec("1111111111111111"), cardFile.read().record());
        CardFile.ReadResult second = cardFile.read();
        assertEquals("00", second.status());
        assertEquals(rec("2222222222222222"), second.record());
        CardFile.ReadResult eof = cardFile.read();
        assertEquals("10", eof.status());
        assertNull(eof.record());
        assertEquals("00", cardFile.close());
    }

    @Test
    void statusesForOperationsOutOfSequence(@TempDir Path dir) throws IOException {
        Path file = dir.resolve("card.txt");
        Files.write(file, List.of(rec("1111111111111111")));
        FixedWidthCardFile cardFile = new FixedWidthCardFile(file);

        assertEquals("47", cardFile.read().status());
        assertEquals("42", cardFile.close());
        assertEquals("00", cardFile.open());
        assertEquals("41", cardFile.open());
    }
}
