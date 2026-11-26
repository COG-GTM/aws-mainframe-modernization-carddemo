package com.carddemo.programs.cobswait;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.util.Optional;

/**
 * Line-sequential SYSIN (one record per text line), e.g. an instream {@code //SYSIN DD *} deck.
 * Only the first {@code lrecl} characters of each line are kept; the rest is skipped without
 * being buffered.
 */
public final class LineSysinReader implements SysinReader {

    /** LRECL of an instream SYSIN card. */
    public static final int SYSIN_LRECL = 80;

    private final BufferedReader reader;
    private final int lrecl;

    public LineSysinReader(Reader reader) {
        this(reader, SYSIN_LRECL);
    }

    public LineSysinReader(Reader reader, int lrecl) {
        if (lrecl <= 0) {
            throw new IllegalArgumentException("lrecl must be positive");
        }
        this.reader = reader instanceof BufferedReader b ? b : new BufferedReader(reader);
        this.lrecl = lrecl;
    }

    public static LineSysinReader of(InputStream in) {
        return new LineSysinReader(new InputStreamReader(in, StandardCharsets.UTF_8));
    }

    @Override
    public Optional<String> acceptRecord() {
        try {
            StringBuilder record = new StringBuilder(lrecl);
            long lineLength = 0;
            int prev = -1;
            int c;
            while ((c = reader.read()) != -1 && c != '\n') {
                if (record.length() < lrecl) {
                    record.append((char) c);
                }
                lineLength++;
                prev = c;
            }
            if (c == -1 && lineLength == 0) {
                return Optional.empty();
            }
            // Drop the CR of a CRLF terminator only if it is the last kept character.
            if (c == '\n' && prev == '\r' && lineLength <= lrecl) {
                record.setLength(record.length() - 1);
            }
            return Optional.of(record.toString());
        } catch (IOException e) {
            throw new UncheckedIOException("SYSIN read failed", e);
        }
    }
}
