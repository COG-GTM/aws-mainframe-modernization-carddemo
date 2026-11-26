package com.carddemo.programs.cbstm03a.io;

import com.carddemo.programs.cbstm03a.support.Pic;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;

/** Reads one fixed-width record per line (the app/data/ASCII format), padding short lines. */
public final class LineSequentialFileReader implements RecordReader {

    private final int recordLength;
    private final BufferedReader in;

    public LineSequentialFileReader(Path path, int recordLength) throws IOException {
        this.recordLength = recordLength;
        this.in = Files.newBufferedReader(path, StandardCharsets.ISO_8859_1);
    }

    @Override
    public Optional<String> read() {
        try {
            String line = in.readLine();
            return line == null ? Optional.empty() : Optional.of(Pic.x(line, recordLength));
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    @Override
    public void close() {
        try {
            in.close();
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}
