package com.carddemo.programs.cobswait;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.util.Optional;

/** Line-sequential SYSIN (one record per text line), e.g. an instream {@code //SYSIN DD *} deck. */
public final class LineSysinReader implements SysinReader {

    private final BufferedReader reader;

    public LineSysinReader(Reader reader) {
        this.reader = reader instanceof BufferedReader b ? b : new BufferedReader(reader);
    }

    public static LineSysinReader of(InputStream in) {
        return new LineSysinReader(new InputStreamReader(in, StandardCharsets.UTF_8));
    }

    @Override
    public Optional<String> acceptRecord() {
        try {
            return Optional.ofNullable(reader.readLine());
        } catch (IOException e) {
            throw new UncheckedIOException("SYSIN read failed", e);
        }
    }
}
