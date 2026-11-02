package com.carddemo.common.record;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.common.record.layout.ExportRecordLayout;
import com.carddemo.common.record.layout.SampleDataset;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

/**
 * Every sample file in {@code app/data} read through its copybook layout and written back produces the
 * same bytes, both as raw records and after decoding every field to a typed value and re-encoding it.
 */
class SampleDataRoundTripTest {

    private static final Map<SampleDataset, Integer> RECORD_COUNTS = Map.ofEntries(
            Map.entry(SampleDataset.ACCTDATA, 50),
            Map.entry(SampleDataset.CARDDATA, 50),
            Map.entry(SampleDataset.CARDXREF, 50),
            Map.entry(SampleDataset.CUSTDATA, 50),
            Map.entry(SampleDataset.DALYTRAN, 300),
            Map.entry(SampleDataset.DISCGRP, 51),
            Map.entry(SampleDataset.TCATBALF, 50),
            Map.entry(SampleDataset.TRANCATG, 18),
            Map.entry(SampleDataset.TRANTYPE, 7),
            Map.entry(SampleDataset.USRSEC, 10),
            Map.entry(SampleDataset.EXPORT, 500));

    @ParameterizedTest
    @EnumSource(value = SampleDataset.class, names = {"USRSEC", "EXPORT"}, mode = EnumSource.Mode.EXCLUDE)
    void asciiFileRoundTripsByteForByte(SampleDataset dataset) throws IOException {
        List<SampleData.Line> lines = SampleData.asciiLines(dataset);
        assertThat(lines).hasSize(RECORD_COUNTS.get(dataset));

        ByteArrayOutputStream out = new ByteArrayOutputStream();
        try (RecordWriter writer = new RecordWriter(out, dataset.asciiFormat().orElseThrow())) {
            for (SampleData.Line line : lines) {
                writer.write(retyped(line.record(), storage(line.record())), line.terminator());
            }
        }
        assertThat(out.toByteArray()).isEqualTo(SampleData.bytes(dataset.asciiPath(SampleData.DATA_DIR).orElseThrow()));
    }

    @ParameterizedTest
    @EnumSource(SampleDataset.class)
    void ebcdicFileRoundTripsByteForByte(SampleDataset dataset) {
        List<CobolRecord> records = SampleData.ebcdic(dataset);
        assertThat(records).hasSize(RECORD_COUNTS.get(dataset));

        List<CobolRecord> retyped = records.stream().map(r -> retyped(r, storage(r))).toList();
        assertThat(RecordWriter.toBytes(retyped, RecordFormat.FIXED))
                .isEqualTo(SampleData.bytes(dataset.ebcdicPath(SampleData.DATA_DIR)));
    }

    @ParameterizedTest
    @EnumSource(value = SampleDataset.class, names = {"USRSEC", "EXPORT"}, mode = EnumSource.Mode.EXCLUDE)
    void asciiFormatWritesTheFileExceptMixedLineEndings(SampleDataset dataset) {
        byte[] written = RecordWriter.toBytes(SampleData.ascii(dataset), dataset.asciiFormat().orElseThrow());
        byte[] source = SampleData.bytes(dataset.asciiPath(SampleData.DATA_DIR).orElseThrow());
        // tcatbal.txt and trantype.txt end their last line with LF, every other line with CRLF.
        boolean lastLineLf = dataset == SampleDataset.TCATBALF || dataset == SampleDataset.TRANTYPE;
        if (lastLineLf) {
            assertThat(written).hasSize(source.length + 1);
            assertThat(new String(written, 0, written.length - 2)).isEqualTo(new String(source, 0, source.length - 1));
        } else {
            assertThat(written).isEqualTo(source);
        }
    }

    private static List<Field> storage(CobolRecord record) {
        if (record.layout() == ExportRecordLayout.LAYOUT) {
            return ExportRecordLayout.storageFor(record.get(ExportRecordLayout.EXPORT_REC_TYPE));
        }
        return record.layout().fields();
    }

    /** Decodes every field of {@code storage} to its Java value and stores it into a fresh record. */
    private static CobolRecord retyped(CobolRecord source, List<Field> storage) {
        CobolRecord copy = CobolRecord.initialized(source.layout(), source.codePage());
        source.values(storage).forEach((field, value) -> {
            if (field instanceof AlphanumericField alpha) {
                copy.set(alpha, (String) value);
            } else {
                copy.set((NumericField) field, (BigDecimal) value);
            }
        });
        return copy;
    }
}
