package com.carddemo.common.batch;

import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.CodePage;
import com.carddemo.common.record.RecordFormat;
import com.carddemo.common.record.RecordLayout;
import com.carddemo.common.record.RecordReader;
import com.carddemo.common.record.RecordWriter;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

/**
 * Reads and writes the file behind a DD with the common record readers/writers. Input organization
 * follows the file name: {@code *.txt} is line sequential (the {@code app/data/ASCII} extracts),
 * anything else fixed-length records as RECFM=F data sets and GnuCOBOL sequential files are.
 */
public final class DdFiles {

    private DdFiles() {
    }

    public static RecordFormat inputFormat(Path file) {
        return file.getFileName().toString().toLowerCase(java.util.Locale.ROOT).endsWith(".txt")
                ? RecordFormat.LINE_SEQUENTIAL : RecordFormat.FIXED;
    }

    public static List<CobolRecord> read(Path file, RecordLayout layout, CodePage codePage) {
        if (!Files.isRegularFile(file)) {
            throw new IllegalStateException("DD file not found: " + file);
        }
        try {
            return RecordReader.readAll(file, layout, codePage, inputFormat(file));
        } catch (IOException ex) {
            throw new UncheckedIOException("Cannot read " + file, ex);
        }
    }

    /** Writes {@code records} as the file of output DD {@code ddName} in the run's generation; returns the count. */
    public static int write(JobRunDirectory generation, String ddName, List<CobolRecord> records, RecordFormat format) {
        Path file = generation.dd(ddName);
        try {
            RecordWriter.writeAll(file, records, format);
        } catch (IOException ex) {
            throw new UncheckedIOException("Cannot write " + file, ex);
        }
        return records.size();
    }
}
