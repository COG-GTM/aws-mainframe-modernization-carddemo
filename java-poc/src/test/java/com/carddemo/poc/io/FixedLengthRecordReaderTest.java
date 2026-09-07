package com.carddemo.poc.io;

import com.carddemo.poc.SampleData;
import com.carddemo.poc.copybook.CardXrefRecord;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
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

    private static FileStatus readStatus(FixedLengthRecordReader reader) {
        reader.readNext();
        return reader.status();
    }
}
