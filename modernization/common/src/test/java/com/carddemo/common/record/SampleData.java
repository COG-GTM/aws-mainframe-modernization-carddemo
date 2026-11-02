package com.carddemo.common.record;

import com.carddemo.common.record.layout.SampleDataset;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/** Reads the sample files under {@code app/data} from the module directory surefire runs in. */
final class SampleData {

    static final Path DATA_DIR = Path.of("..", "..", "app", "data");

    private SampleData() {
    }

    static List<CobolRecord> ascii(SampleDataset dataset) {
        return read(dataset.asciiPath(DATA_DIR).orElseThrow(), dataset, CodePage.ASCII,
                dataset.asciiFormat().orElseThrow());
    }

    static List<CobolRecord> ebcdic(SampleDataset dataset) {
        return read(dataset.ebcdicPath(DATA_DIR), dataset, CodePage.EBCDIC, RecordFormat.FIXED);
    }

    static byte[] bytes(Path file) {
        try {
            return Files.readAllBytes(file);
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }

    /** Line-sequential records with the terminator each one was read with. */
    record Line(CobolRecord record, String terminator) {
    }

    static List<Line> asciiLines(SampleDataset dataset) {
        Path file = dataset.asciiPath(DATA_DIR).orElseThrow();
        try (RecordReader reader = new RecordReader(Files.newInputStream(file), dataset.layout(), CodePage.ASCII,
                dataset.asciiFormat().orElseThrow())) {
            List<Line> lines = new ArrayList<>();
            for (CobolRecord record = reader.read(); record != null; record = reader.read()) {
                lines.add(new Line(record, reader.lineEnding()));
            }
            return lines;
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }

    private static List<CobolRecord> read(Path file, SampleDataset dataset, CodePage codePage, RecordFormat format) {
        try {
            return RecordReader.readAll(file, dataset.layout(), codePage, format);
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }
}
