package com.carddemo.programs.cbstm03b.io;

import java.io.IOException;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

/** Writes RECFM=FB records as line-delimited text, one LRECL-byte record per line. */
public final class FixedWidthRecordWriter {

    private final int lrecl;

    public FixedWidthRecordWriter(int lrecl) {
        this.lrecl = lrecl;
    }

    public void writeAll(Path path, List<String> records) throws IOException {
        try (Writer w = Files.newBufferedWriter(path, StandardCharsets.US_ASCII)) {
            writeAll(w, records);
        }
    }

    public void writeAll(Writer writer, List<String> records) throws IOException {
        for (String rec : records) {
            if (rec.length() != lrecl) {
                throw new IOException("record is " + rec.length() + " bytes, LRECL=" + lrecl);
            }
            writer.write(rec);
            writer.write('\n');
        }
    }
}
