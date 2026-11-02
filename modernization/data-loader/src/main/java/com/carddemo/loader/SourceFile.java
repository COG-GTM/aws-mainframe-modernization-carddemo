package com.carddemo.loader;

import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.CodePage;
import com.carddemo.common.record.RecordFormat;
import com.carddemo.common.record.RecordReader;
import com.carddemo.common.record.layout.SampleDataset;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * The file a dataset is loaded from: its ASCII extract under {@code app/data/ASCII}, or the EBCDIC
 * {@code .PS} file for USRSEC, which has no ASCII copy. Keeps each line's terminator so the
 * records can be written back exactly.
 */
record SourceFile(SampleDataset dataset, Path path, CodePage codePage, RecordFormat format,
                  List<CobolRecord> records, List<String> terminators) {

    static SourceFile read(Path dataDir, SampleDataset dataset) throws IOException {
        if (dataset.asciiPath(dataDir).isPresent()) {
            return read(dataset, dataset.asciiPath(dataDir).get(), CodePage.ASCII, dataset.asciiFormat().orElseThrow());
        }
        return read(dataset, dataset.ebcdicPath(dataDir), CodePage.EBCDIC, RecordFormat.FIXED);
    }

    static SourceFile readEbcdic(Path dataDir, SampleDataset dataset) throws IOException {
        return read(dataset, dataset.ebcdicPath(dataDir), CodePage.EBCDIC, RecordFormat.FIXED);
    }

    private static SourceFile read(SampleDataset dataset, Path path, CodePage codePage, RecordFormat format)
            throws IOException {
        try (InputStream in = Files.newInputStream(path)) {
            return read(dataset, path, codePage, format, in);
        }
    }

    static SourceFile read(SampleDataset dataset, Path path, CodePage codePage, RecordFormat format, InputStream in)
            throws IOException {
        List<CobolRecord> records = new ArrayList<>();
        List<String> terminators = new ArrayList<>();
        try (RecordReader reader = new RecordReader(in, dataset.layout(), codePage, format)) {
            for (CobolRecord record = reader.read(); record != null; record = reader.read()) {
                records.add(record);
                terminators.add(reader.lineEnding());
            }
        }
        return new SourceFile(dataset, path, codePage, format, List.copyOf(records), List.copyOf(terminators));
    }

    /** {@code ASCII/acctdata.txt} or {@code EBCDIC/AWS.M2.CARDDEMO.USRSEC.PS}. */
    String displayName() {
        return path.getParent().getFileName() + "/" + path.getFileName();
    }
}
