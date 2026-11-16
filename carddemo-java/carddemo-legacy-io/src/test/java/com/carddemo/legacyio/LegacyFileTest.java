package com.carddemo.legacyio;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.legacyio.copybook.Copybook;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;

class LegacyFileTest {

    private static final RecordLayout THREE = RecordLayout.of(Copybook.parse("T",
            "       01  T-REC.\n           05  N  PIC S9(3)."));

    @Test
    void rejectsExportRecordsWithALineFeedByteInLineSequentialFormat() throws IOException {
        RecordLayout account = CardDemoFile.EXPORT.layout("EXPORT-ACCOUNT-DATA");
        Record record = Record.blank(account).set("EXPORT-REC-TYPE", "A").set("EXPORT-SEQUENCE-NUM", 10);

        assertThatThrownBy(() -> LegacyFile.toBytes(List.of(record), Encoding.ASCII, RecordFormat.LINE_SEQUENTIAL))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("line-feed")
                .hasMessageContaining("FIXED");

        byte[] fixed = LegacyFile.toBytes(List.of(record), Encoding.ASCII, RecordFormat.FIXED);
        List<Record> reread = LegacyFile.read(new ByteArrayInputStream(fixed), CardDemoFile.EXPORT, Encoding.ASCII,
                RecordFormat.FIXED);
        assertThat(reread).hasSize(1);
        assertThat(reread.get(0).getLong("EXPORT-SEQUENCE-NUM")).isEqualTo(10);
    }

    @Test
    void rejectsRecordsEndingInCarriageReturnBeforeALineEnding() {
        RecordLayout text = RecordLayout.of(Copybook.parse("X", "       01  X-REC.\n           05  S  PIC X(3)."));
        Record record = Record.blank(text).set("S", "AB\r");

        assertThatThrownBy(() -> LegacyFile.toBytes(List.of(record), Encoding.ASCII, RecordFormat.LINE_SEQUENTIAL))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("carriage-return");
    }

    @Test
    void terminatesAnUnterminatedLineWhenRecordsFollowIt() throws IOException {
        List<Record> records = new ArrayList<>(read("12C"));
        assertThat(records.get(0).framing().terminator()).isEqualTo(LineFraming.Terminator.NONE);
        records.add(Record.blank(THREE));

        byte[] written = LegacyFile.toBytes(records, Encoding.ASCII, RecordFormat.LINE_SEQUENTIAL);

        assertThat(new String(written, StandardCharsets.ISO_8859_1)).isEqualTo("12C\n00{\n");
        assertThat(read(new String(written, StandardCharsets.ISO_8859_1))).hasSize(2);
    }

    @Test
    void keepsAMissingLineEndingOnTheLastRecord() throws IOException {
        List<Record> records = new ArrayList<>(read("12C\n"));
        records.addAll(read("45D"));

        byte[] written = LegacyFile.toBytes(records, Encoding.ASCII, RecordFormat.LINE_SEQUENTIAL);

        assertThat(new String(written, StandardCharsets.ISO_8859_1)).isEqualTo("12C\n45D");
        assertThat(records.get(1).getDecimal("N")).isEqualTo(new BigDecimal("454"));
    }

    private static List<Record> read(String text) throws IOException {
        return LegacyFile.read(new ByteArrayInputStream(text.getBytes(StandardCharsets.ISO_8859_1)), 3,
                RecordFormat.LINE_SEQUENTIAL, bytes -> new RecordCodec(THREE, Encoding.ASCII));
    }
}
