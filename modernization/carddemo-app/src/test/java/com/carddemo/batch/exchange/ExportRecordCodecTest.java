package com.carddemo.batch.exchange;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.load.VsamDatasetLoader.Dataset;
import com.carddemo.common.codec.Field;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.codec.TestData;
import com.carddemo.support.Samples;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Arrays;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

/** CBEXPORT/CBIMPORT record moves over the shipped samples, without a database. */
class ExportRecordCodecTest {

    private static final String TS = "2022-07-06 00:00:00.00";

    @Test
    void layoutIsTheFiveHundredByteCvexportRecord() {
        assertThat(ExportRecordCodec.LAYOUT.length()).isEqualTo(500);
    }

    @ParameterizedTest
    @EnumSource(ExportRecordType.class)
    void exportThenImportGivesBackTheDatasetRecordByteForByte(ExportRecordType type) {
        for (RecordEncoding encoding : List.of(RecordEncoding.EBCDIC, RecordEncoding.ASCII)) {
            List<FixedWidthRecord> source = sample(type, encoding);
            assertThat(source).isNotEmpty();
            long seq = 0;
            for (FixedWidthRecord record : source) {
                FixedWidthRecord export = ExportRecordCodec.export(type, record, TS, ++seq);
                assertThat(export.length()).isEqualTo(500);
                assertThat(ExportRecordCodec.recordType(export)).isEqualTo(type.code());
                assertThat(ExportRecordCodec.sequence(export)).isEqualTo(seq);
                assertThat(export.getString("EXPORT-BRANCH-ID")).isEqualTo("0001");
                assertThat(export.getString("EXPORT-REGION-CODE")).isEqualTo("NORTH");
                assertThat(ExportRecordCodec.importRecord(type, export).bytes())
                        .as("%s %s record %d", type, encoding, seq).isEqualTo(record.bytes());
            }
        }
    }

    /**
     * The GnuCOBOL CBEXPORT run (docs/validation/baseline/CBEXPORT) wrote customers, accounts and xrefs first, so
     * sequence numbers 1-150 line up. It ran after POSTTRAN/INTCALC in the baseline sequence, so the account fields
     * those jobs rewrite (current balance, cycle credit/debit) are masked; everything else must match byte for byte.
     */
    @Test
    void asciiExportMatchesTheGnuCobolCbexportRecordsForCustomersAccountsAndXrefs() throws IOException {
        List<String> baseline = Files.readAllLines(TestData.resolve("docs/validation/baseline/CBEXPORT/EXPORT.ksds.txt"),
                StandardCharsets.ISO_8859_1);
        List<Field> postedByBatch = List.of(ExportRecordCodec.LAYOUT.field("EXP-ACCT-CURR-BAL"),
                ExportRecordCodec.LAYOUT.field("EXP-ACCT-CURR-CYC-CREDIT"),
                ExportRecordCodec.LAYOUT.field("EXP-ACCT-CURR-CYC-DEBIT"));
        long seq = 0;
        for (ExportRecordType type : List.of(ExportRecordType.CUSTOMER, ExportRecordType.ACCOUNT,
                ExportRecordType.CARD_XREF)) {
            for (FixedWidthRecord record : Samples.read(type.dataset(), RecordEncoding.ASCII)) {
                byte[] ours = ExportRecordCodec.export(type, record, TS, ++seq).bytes();
                byte[] cobol = unrender(baseline.get((int) seq - 1), ours.length);
                if (type == ExportRecordType.ACCOUNT) {
                    postedByBatch.forEach(f -> {
                        Arrays.fill(ours, f.offset(), f.offset() + f.size(), (byte) 0);
                        Arrays.fill(cobol, f.offset(), f.offset() + f.size(), (byte) 0);
                    });
                }
                assertThat(ours).as("%s seq %d", type, seq).isEqualTo(cobol);
            }
        }
        assertThat(seq).isEqualTo(150);
    }

    /** Inverse of scripts/baseline/baseline.py {@code render}: {@code \\xNN} escapes, trailing spaces restored. */
    static byte[] unrender(String line, int length) {
        byte[] out = new byte[length];
        Arrays.fill(out, (byte) ' ');
        int n = 0;
        for (int i = 0; i < line.length() && n < length; n++) {
            if (line.startsWith("\\x", i)) {
                out[n] = (byte) Integer.parseInt(line.substring(i + 2, i + 4), 16);
                i += 4;
            } else {
                out[n] = (byte) line.charAt(i++);
            }
        }
        return out;
    }

    @Test
    void errorRecordFollowsCbimportWsErrorRecord() {
        FixedWidthRecord export = FixedWidthRecord.spaces(ExportRecordCodec.LAYOUT, RecordEncoding.ASCII);
        export.setString("EXPORT-REC-TYPE", "Z");
        export.setLong("EXPORT-SEQUENCE-NUM", 123_456_789L);
        FixedWidthRecord error = ExportRecordCodec.error(export, "Unknown record type encountered",
                ZonedDateTime.of(LocalDateTime.of(2022, 7, 6, 1, 2, 3, 450_000_000), ZoneOffset.UTC));
        String text = new String(error.bytes(), StandardCharsets.ISO_8859_1);
        assertThat(text).hasSize(132);
        assertThat(text).startsWith("2022070601020345+0000     |Z|3456789|Unknown record type encountered");
        assertThat(text.substring(37 + 50)).isBlank();
    }

    private static List<FixedWidthRecord> sample(ExportRecordType type, RecordEncoding encoding) {
        if (type.dataset() != Dataset.TRANSACT) {
            return Samples.read(type.dataset(), encoding);
        }
        // TRANSACT ships only as a priming record; CVTRA06Y (DALYTRAN) has the same 350-byte layout as CVTRA05Y.
        return Samples.read(Dataset.DALYTRAN, encoding).stream()
                .map(r -> new FixedWidthRecord(type.datasetLayout(), r.bytes(), encoding)).toList();
    }
}
