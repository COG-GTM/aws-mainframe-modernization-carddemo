package com.carddemo.interestcalc.file;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Stream;

/** Reads the fixed-width ASCII datasets that stand in for the VSAM KSDS files. */
public final class DatasetFiles {

    private DatasetFiles() {
    }

    /**
     * Reads a fixed-width dataset. Blank lines are skipped and short lines are space padded,
     * which is what the shipped {@code app/data/ASCII} files need: {@code cardxref.txt} omits
     * the trailing FILLER and {@code tcatbal.txt} uses CRLF line endings.
     */
    public static <T> List<T> read(Path file, Function<String, T> parser) {
        try (Stream<String> lines = Files.lines(file, StandardCharsets.ISO_8859_1)) {
            return lines.filter(line -> !line.isBlank()).map(parser).toList();
        } catch (IOException e) {
            throw new UncheckedIOException("cannot read dataset " + file, e);
        }
    }

    /** Writes fixed-width records, one per line. */
    public static void write(Path file, List<String> records) {
        try {
            Path parent = file.getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
            Files.writeString(file, String.join("\n", records) + (records.isEmpty() ? "" : "\n"),
                    StandardCharsets.ISO_8859_1);
        } catch (IOException e) {
            throw new UncheckedIOException("cannot write " + file, e);
        }
    }
}
