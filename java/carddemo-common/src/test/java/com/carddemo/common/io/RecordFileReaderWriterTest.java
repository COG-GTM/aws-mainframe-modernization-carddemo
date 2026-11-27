package com.carddemo.common.io;

import com.carddemo.common.cobol.CobolDataException;
import com.carddemo.common.cobol.DataEncoding;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class RecordFileReaderWriterTest {

    @TempDir
    Path dir;

    @Test
    void fixedRoundTrip() {
        Path f = dir.resolve("fb.dat");
        try (RecordFileWriter w = RecordFileWriter.create(f, RecordFormat.FIXED, 4)) {
            w.write("ABCD".getBytes(StandardCharsets.US_ASCII));
            w.write("EFGH".getBytes(StandardCharsets.US_ASCII));
        }
        List<byte[]> records = RecordFileReader.readAll(f, RecordFormat.FIXED, 4, DataEncoding.ASCII);
        assertThat(records).extracting(b -> new String(b, StandardCharsets.US_ASCII)).containsExactly("ABCD", "EFGH");
    }

    @Test
    void variableRoundTripWithRdw() throws Exception {
        Path f = dir.resolve("vb.dat");
        try (RecordFileWriter w = RecordFileWriter.create(f, RecordFormat.VARIABLE, 100)) {
            w.write(new byte[] {1, 2, 3});
            w.write(new byte[] {4});
        }
        assertThat(Files.readAllBytes(f)).containsExactly(0, 7, 0, 0, 1, 2, 3, 0, 5, 0, 0, 4);
        assertThat(RecordFileReader.readAll(f, RecordFormat.VARIABLE, 100, DataEncoding.EBCDIC))
                .containsExactly(new byte[] {1, 2, 3}, new byte[] {4});
    }

    @Test
    void linesArePaddedToLrecl() throws Exception {
        Path f = dir.resolve("text.txt");
        Files.writeString(f, "AB\r\nC\nDEF", StandardCharsets.ISO_8859_1);
        assertThat(RecordFileReader.readAll(f, RecordFormat.LINE, 3, DataEncoding.ASCII))
                .extracting(b -> new String(b, StandardCharsets.ISO_8859_1)).containsExactly("AB ", "C  ", "DEF");
        Files.writeString(f, "TOOLONG\n", StandardCharsets.ISO_8859_1);
        assertThatThrownBy(() -> RecordFileReader.readAll(f, RecordFormat.LINE, 3, DataEncoding.ASCII))
                .isInstanceOf(CobolDataException.class);
    }

    @Test
    void shortFixedRecordIsAnError() throws Exception {
        Path f = dir.resolve("short.dat");
        Files.write(f, new byte[] {1, 2, 3, 4, 5});
        assertThatThrownBy(() -> RecordFileReader.readAll(f, RecordFormat.FIXED, 4, DataEncoding.EBCDIC))
                .isInstanceOf(CobolDataException.class);
    }
}
