package com.carddemo.poc.io;

import com.carddemo.poc.SampleData;
import com.carddemo.poc.copybook.CardXrefRecord;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class FixedLengthRecordReaderTest {

    private static List<String> readAll(Path file) {
        List<String> out = new ArrayList<>();
        try (FixedLengthRecordReader reader = FixedLengthRecordReader.forDataFile(file, CardXrefRecord.RECORD_LENGTH)) {
            assertEquals(FileStatus.SUCCESS, reader.open());
            Optional<byte[]> rec;
            while ((rec = reader.readNext()).isPresent()) {
                out.add(new String(rec.get(), StandardCharsets.ISO_8859_1));
            }
            assertEquals(FileStatus.END_OF_FILE, reader.status());
        }
        return out;
    }

    @Test
    void ebcdicAndAsciiSampleFilesDecodeIdentically() {
        List<String> ebcdic = readAll(SampleData.ebcdic("AWS.M2.CARDDEMO.CARDXREF.PS"));
        List<String> ascii = readAll(SampleData.ascii("cardxref.txt"));
        assertEquals(50, ebcdic.size());
        assertEquals(ascii, ebcdic);
        assertTrue(ebcdic.stream().allMatch(r -> r.length() == CardXrefRecord.RECORD_LENGTH));
    }

    @Test
    void reportsFileStatusInsteadOfThrowing() {
        FixedLengthRecordReader reader = FixedLengthRecordReader.forDataFile(Path.of("does-not-exist.PS"), 50);
        assertEquals(FileStatus.READ_NOT_OPEN, readStatus(reader));
        assertEquals(FileStatus.FILE_NOT_FOUND, reader.open());
        reader.close();
        assertEquals(FileStatus.NOT_OPEN, reader.status());
    }

    @Test
    void oversizedAsciiLineIsConsumedBeforeReportingError(@TempDir Path dir) throws IOException {
        Path file = dir.resolve("bad.txt");
        Files.writeString(file, "X".repeat(12) + "\n" + "Y".repeat(5) + "\n", StandardCharsets.ISO_8859_1);
        try (FixedLengthRecordReader reader = FixedLengthRecordReader.forDataFile(file, 8)) {
            reader.open();
            assertTrue(reader.readNext().isEmpty());
            assertEquals(FileStatus.IO_ERROR, reader.status());
            assertEquals("YYYYY   ", new String(reader.readNext().orElseThrow(), StandardCharsets.ISO_8859_1));
            assertTrue(reader.readNext().isEmpty());
            assertEquals(FileStatus.END_OF_FILE, reader.status());
        }
    }

    private static FileStatus readStatus(FixedLengthRecordReader reader) {
        reader.readNext();
        return reader.status();
    }
}
