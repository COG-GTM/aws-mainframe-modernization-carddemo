package com.carddemo.common.file;

import com.carddemo.common.codec.Copybook;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.codec.RecordLayout;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class RecordFilesTest {

    private static final RecordLayout XREF = Copybook.layout("CVACT03Y");

    @TempDir
    Path dir;

    @Test
    void fixedFilesRoundTripAndLengthIsChecked() throws IOException {
        FixedWidthRecord a = FixedWidthRecord.spaces(XREF, RecordEncoding.EBCDIC);
        a.setString("XREF-CARD-NUM", "4111111111111111");
        a.setLong("XREF-CUST-ID", 1);
        Path file = dir.resolve("out/xref.ps");
        RecordFiles.writeFixed("XREFFILE", file, List.of(a, a.copy()));
        assertThat(Files.size(file)).isEqualTo(100);
        List<FixedWidthRecord> read = RecordFiles.readFixed("XREFFILE", file, XREF, RecordEncoding.EBCDIC);
        assertThat(read).containsExactly(a, a);
        Files.write(file, new byte[51]);
        assertThatThrownBy(() -> RecordFiles.readFixed("XREFFILE", file, XREF, RecordEncoding.EBCDIC))
                .isInstanceOfSatisfying(FileStatusException.class,
                        e -> assertThat(e.status()).isEqualTo(FileStatus.RECORD_LENGTH_MISMATCH));
        assertThatThrownBy(() -> RecordFiles.readFixed("XREFFILE", dir.resolve("missing"), XREF,
                RecordEncoding.EBCDIC)).isInstanceOfSatisfying(FileStatusException.class,
                e -> assertThat(e.status()).isEqualTo(FileStatus.FILE_NOT_FOUND));
        assertThatThrownBy(() -> RecordFiles.readFixed("XREFFILE", dir, XREF, RecordEncoding.EBCDIC))
                .isInstanceOfSatisfying(FileStatusException.class,
                        e -> assertThat(e.status()).isEqualTo(FileStatus.PERMANENT_ERROR));
    }

    @Test
    void lineSequentialPadsShortLinesAndAcceptsCrlf() throws IOException {
        Path file = dir.resolve("xref.txt");
        Files.writeString(file, "4111111111111111000000001\r\nABC\n", StandardCharsets.ISO_8859_1);
        List<FixedWidthRecord> read = RecordFiles.readLines("XREF", file, XREF, RecordEncoding.ASCII);
        assertThat(read).hasSize(2);
        assertThat(read.get(0).getLong("XREF-CUST-ID")).isEqualTo(1);
        assertThat(read.get(1).text()).isEqualTo("ABC" + " ".repeat(47));
        Files.writeString(file, "no newline at end");
        assertThat(RecordFiles.readLines("XREF", file, XREF, RecordEncoding.ASCII)).hasSize(1);
        Files.writeString(file, "");
        assertThat(RecordFiles.readLines("XREF", file, XREF, RecordEncoding.ASCII)).isEmpty();
        Files.writeString(file, "X".repeat(51) + "\n");
        assertThatThrownBy(() -> RecordFiles.readLines("XREF", file, XREF, RecordEncoding.ASCII))
                .isInstanceOfSatisfying(FileStatusException.class,
                        e -> assertThat(e.status()).isEqualTo(FileStatus.RECORD_LENGTH_ERROR));
        Path out = dir.resolve("lines.txt");
        RecordFiles.writeLines("OUT", out, read, true);
        assertThat(Files.readString(out)).isEqualTo("4111111111111111000000001\nABC\n");
        RecordFiles.writeLines("OUT", out, read.subList(1, 2), false);
        assertThat(Files.readString(out)).isEqualTo("ABC" + " ".repeat(47) + "\n");
        assertThatThrownBy(() -> RecordFiles.writeLines("OUT", dir, read, true))
                .isInstanceOf(FileStatusException.class);
    }

    @Test
    void variableFilesWithEachPrefix() {
        for (RecordPrefix prefix : List.of(RecordPrefix.GNUCOBOL_VARSEQ, RecordPrefix.ZOS_RDW)) {
            Path file = dir.resolve(prefix + ".dat");
            VariableRecordWriter w = new VariableRecordWriter("OUT", file, prefix, 1, 10);
            w.open();
            w.write("ABCDEFGHIJ".getBytes(StandardCharsets.US_ASCII), 3);
            w.write("XYZ".getBytes(StandardCharsets.US_ASCII), 1);
            w.close();
            List<byte[]> records = RecordFiles.readVariable("OUT", file, prefix);
            assertThat(records).extracting(b -> new String(b, StandardCharsets.US_ASCII)).containsExactly("ABC", "X");
        }
        assertThatThrownBy(() -> RecordFiles.readVariable("OUT", dir.resolve("x"), RecordPrefix.NONE))
                .isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void truncatedVariableFilesAreLengthMismatches() throws IOException {
        Path file = dir.resolve("bad.dat");
        Files.write(file, new byte[] {0, 0, 0, 9, 'A'});
        assertThatThrownBy(() -> RecordFiles.readVariable("IN", file, RecordPrefix.GNUCOBOL_VARSEQ))
                .isInstanceOf(FileStatusException.class);
        Files.write(file, new byte[] {0x7F, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, 'A'});
        assertThatThrownBy(() -> RecordFiles.readVariable("IN", file, RecordPrefix.GNUCOBOL_VARSEQ))
                .isInstanceOfSatisfying(FileStatusException.class,
                        e -> assertThat(e.status()).isEqualTo(FileStatus.RECORD_LENGTH_MISMATCH));
        Files.write(file, new byte[] {0, 0});
        assertThatThrownBy(() -> RecordFiles.readVariable("IN", file, RecordPrefix.ZOS_RDW))
                .isInstanceOf(FileStatusException.class);
        Files.write(file, new byte[] {0, 2, 0, 0});
        assertThatThrownBy(() -> RecordFiles.readVariable("IN", file, RecordPrefix.ZOS_RDW))
                .isInstanceOf(FileStatusException.class);
    }
}
