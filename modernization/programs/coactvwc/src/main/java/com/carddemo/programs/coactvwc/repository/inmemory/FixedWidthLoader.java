package com.carddemo.programs.coactvwc.repository.inmemory;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.function.Function;

/** Loads line-sequential fixed-width ASCII extracts (app/data/ASCII) into copybook records. */
public final class FixedWidthLoader {

    private FixedWidthLoader() {
    }

    public static <T> List<T> load(Path file, Function<String, T> parser) {
        try {
            return Files.readAllLines(file, StandardCharsets.ISO_8859_1).stream()
                    .filter(l -> !l.isBlank())
                    .map(parser)
                    .toList();
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}
