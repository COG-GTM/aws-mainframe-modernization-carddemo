package com.carddemo.common.record;

import com.carddemo.common.cobol.CodePage;
import com.carddemo.common.error.FileStatus;
import com.carddemo.common.error.FileStatusException;
import com.carddemo.common.error.NumericDataException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class RecordLayoutTest {

    /** CVTRA06Y (DALYTRAN, 350 bytes), declared here only as a fixture for the layout base. */
    static final RecordLayout DALYTRAN = RecordLayout.builder("CVTRA06Y", 350)
            .alphanumeric("DALYTRAN-ID", 16)
            .alphanumeric("DALYTRAN-TYPE-CD", 2)
            .zoned("DALYTRAN-CAT-CD", 4, 0, false)
            .alphanumeric("DALYTRAN-SOURCE", 10)
            .alphanumeric("DALYTRAN-DESC", 100)
            .zoned("DALYTRAN-AMT", 9, 2, true)
            .zoned("DALYTRAN-MERCHANT-ID", 9, 0, false)
            .alphanumeric("DALYTRAN-MERCHANT-NAME", 50)
            .alphanumeric("DALYTRAN-MERCHANT-CITY", 50)
            .alphanumeric("DALYTRAN-MERCHANT-ZIP", 10)
            .alphanumeric("DALYTRAN-CARD-NUM", 16)
            .alphanumeric("DALYTRAN-ORIG-TS", 26)
            .alphanumeric("DALYTRAN-PROC-TS", 26)
            .filler(20)
            .build();

    static final Path REPO = Path.of("..", "..");

    @Test
    void offsetsFollowCopybookOrder() {
        assertThat(DALYTRAN.field("DALYTRAN-AMT").position()).isEqualTo(133);
        assertThat(DALYTRAN.field("DALYTRAN-AMT").length()).isEqualTo(11);
        assertThat(DALYTRAN.field("DALYTRAN-CARD-NUM").position()).isEqualTo(263);
        assertThat(DALYTRAN.fields()).hasSize(14);
    }

    @Test
    void layoutMustTileTheRecord() {
        assertThatThrownBy(() -> RecordLayout.builder("SHORT", 10).alphanumeric("A", 5).build())
                .hasMessageContaining("cover 5 bytes");
        assertThatThrownBy(() -> RecordLayout.builder("DUP", 4).alphanumeric("A", 2).alphanumeric("A", 2).build())
                .hasMessageContaining("duplicate");
        assertThatThrownBy(() -> DALYTRAN.field("NOPE")).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void packedAndBinaryFieldsUseStorageLength() {
        RecordLayout layout = RecordLayout.builder("MIXED", 12)
                .packed("P", 9, 2, true)
                .binary("B", 4, 0, true)
                .binary("L", 9, 0, false)
                .build();
        CobolRecord record = layout.blank(CodePage.CP037)
                .setDecimal("P", new BigDecimal("-504.77"))
                .setDecimal("B", new BigDecimal("-2"))
                .setLong("L", 123456789L);
        assertThat(record.rawField("P")).containsExactly(0x00, 0x00, 0x00, 0x50, 0x47, 0x7D);
        CobolRecord back = layout.wrap(record.bytes(), CodePage.CP037);
        assertThat(back.getDecimal("P")).isEqualTo(new BigDecimal("-504.77"));
        assertThat(back.getDecimal("B")).isEqualTo(new BigDecimal("-2"));
        assertThat(back.getLong("L")).isEqualTo(123456789L);
    }

    @Test
    void setStringPadsAndTruncatesLikeMove() {
        RecordLayout layout = RecordLayout.builder("X", 6).alphanumeric("A", 4).alphanumeric("B", 2).build();
        CobolRecord record = layout.blank(CodePage.ASCII).setString("A", "AB").setString("B", "XYZ");
        assertThat(new String(record.bytes(), StandardCharsets.US_ASCII)).isEqualTo("AB  XY");
        assertThat(record.getTrimmed("A")).isEqualTo("AB");
        assertThat(layout.blank(CodePage.ASCII).getTrimmed("A")).isNull();
    }

    @Test
    void readsTheAsciiAndEbcdicSampleDalytranIdentically() {
        List<CobolRecord> ascii = readAll(REPO.resolve("app/data/ASCII/dailytran.txt"), CodePage.ASCII, RecordFormat.LINE_SEQUENTIAL);
        List<CobolRecord> ebcdic = readAll(REPO.resolve("app/data/EBCDIC/AWS.M2.CARDDEMO.DALYTRAN.PS"), CodePage.CP037,
                RecordFormat.FIXED_BLOCK);
        assertThat(ascii).hasSize(300);
        assertThat(ebcdic).hasSize(300);
        for (int i = 0; i < ascii.size(); i++) {
            for (Field field : DALYTRAN.fields()) {
                if (field.type() == FieldType.FILLER) {
                    continue;
                }
                assertThat(ebcdic.get(i).getString(field.name())).as("record %d %s", i + 1, field.name())
                        .isEqualTo(ascii.get(i).getString(field.name()));
            }
            assertThat(ebcdic.get(i).getDecimal("DALYTRAN-AMT")).isEqualTo(ascii.get(i).getDecimal("DALYTRAN-AMT"));
            assertThat(CodePage.CP037.convert(ebcdic.get(i).bytes(), CodePage.ASCII)).containsExactly(ascii.get(i).bytes());
        }
        CobolRecord first = ascii.get(0);
        assertThat(first.getString("DALYTRAN-ID")).isEqualTo("0000000000683580");
        assertThat(first.getLong("DALYTRAN-CAT-CD")).isEqualTo(1);
        assertThat(first.getDecimal("DALYTRAN-AMT")).isEqualTo(new BigDecimal("504.77"));
    }

    @Test
    void writerRoundTripsBytesExactly(@TempDir Path dir) throws Exception {
        List<CobolRecord> records = readAll(REPO.resolve("app/data/EBCDIC/AWS.M2.CARDDEMO.DALYTRAN.PS"), CodePage.CP037,
                RecordFormat.FIXED_BLOCK);
        Path out = dir.resolve("copy.PS");
        try (RecordWriter writer = RecordWriter.open(out, "DALYTRAN", DALYTRAN, RecordFormat.FIXED_BLOCK)) {
            records.forEach(writer::write);
            assertThat(writer.recordsWritten()).isEqualTo(300);
        }
        assertThat(Files.readAllBytes(out))
                .containsExactly(Files.readAllBytes(REPO.resolve("app/data/EBCDIC/AWS.M2.CARDDEMO.DALYTRAN.PS")));
    }

    @Test
    void lineSequentialPadsShortLinesAndStripsCr() {
        RecordLayout layout = RecordLayout.builder("L", 5).alphanumeric("A", 5).build();
        RecordReader reader = RecordReader.of(new ByteArrayInputStream("AB\r\nABCDE\n\nX".getBytes(StandardCharsets.US_ASCII)),
                "IN", layout, CodePage.ASCII, RecordFormat.LINE_SEQUENTIAL);
        assertThat(reader.read().orElseThrow().getString("A")).isEqualTo("AB   ");
        assertThat(reader.read().orElseThrow().getString("A")).isEqualTo("ABCDE");
        assertThat(reader.read().orElseThrow().getString("A")).isEqualTo("     ");
        assertThat(reader.read().orElseThrow().getString("A")).isEqualTo("X    ");
        assertThat(reader.read()).isEmpty();
        assertThat(reader.recordsRead()).isEqualTo(4);
    }

    @Test
    void wrongLengthsAbendInsteadOfSkipping() {
        RecordLayout layout = RecordLayout.builder("L", 4).alphanumeric("A", 4).build();
        RecordReader longLine = RecordReader.of(new ByteArrayInputStream("ABCDE\n".getBytes(StandardCharsets.US_ASCII)),
                "IN", layout, CodePage.ASCII, RecordFormat.LINE_SEQUENTIAL);
        assertThatThrownBy(longLine::read).isInstanceOf(FileStatusException.class)
                .satisfies(ex -> assertThat(((FileStatusException) ex).status()).isEqualTo(FileStatus.RECORD_LENGTH_CONFLICT));
        RecordReader partial = RecordReader.of(new ByteArrayInputStream(new byte[6]), "IN", layout, CodePage.CP037,
                RecordFormat.FIXED_BLOCK);
        assertThat(partial.read()).isPresent();
        assertThatThrownBy(partial::read).isInstanceOf(FileStatusException.class);
        RecordWriter writer = RecordWriter.of(new ByteArrayOutputStream(), "OUT", layout, RecordFormat.FIXED_BLOCK);
        assertThatThrownBy(() -> writer.write(new byte[3])).isInstanceOf(FileStatusException.class)
                .satisfies(ex -> assertThat(((FileStatusException) ex).status()).isEqualTo(FileStatus.WRITE_LENGTH_ERROR));
        assertThatThrownBy(() -> layout.wrap(new byte[5], CodePage.ASCII)).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void missingFileIsStatus35(@TempDir Path dir) {
        assertThatThrownBy(() -> RecordReader.open(dir.resolve("none"), "DALYTRAN", DALYTRAN, CodePage.ASCII,
                RecordFormat.LINE_SEQUENTIAL))
                .isInstanceOf(FileStatusException.class)
                .hasMessageContaining("FILE STATUS 35");
    }

    @Test
    void badNumericBytesInARecordAbend() {
        CobolRecord record = DALYTRAN.blank(CodePage.ASCII);
        assertThatThrownBy(() -> record.getDecimal("DALYTRAN-AMT")).isInstanceOf(NumericDataException.class);
    }

    @Test
    void lineSequentialWriterAppendsNewline() {
        RecordLayout layout = RecordLayout.builder("L", 3).alphanumeric("A", 3).build();
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        try (RecordWriter writer = RecordWriter.of(out, "OUT", layout, RecordFormat.LINE_SEQUENTIAL)) {
            writer.write(layout.blank(CodePage.ASCII).setString("A", "AB"));
        }
        assertThat(out.toString(StandardCharsets.US_ASCII)).isEqualTo("AB \n");
    }

    private static List<CobolRecord> readAll(Path file, CodePage codePage, RecordFormat format) {
        List<CobolRecord> records = new ArrayList<>();
        try (RecordReader reader = RecordReader.open(file, "DALYTRAN", DALYTRAN, codePage, format)) {
            for (Optional<CobolRecord> r = reader.read(); r.isPresent(); r = reader.read()) {
                records.add(r.get());
            }
        }
        return records;
    }
}
