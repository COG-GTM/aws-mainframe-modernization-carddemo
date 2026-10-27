package com.carddemo.modernization.batch;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

/** Fixed-length sequential datasets: no record separators, every record exactly {@code length} bytes. */
final class FixedLengthFiles {
    private FixedLengthFiles() {}

    static List<byte[]> read(Path file, int length) {
        byte[] bytes = readAll(file);
        if (bytes.length % length != 0) {
            throw new IllegalStateException(file + " is " + bytes.length + " bytes, not a multiple of LRECL " + length);
        }
        List<byte[]> records = new ArrayList<>(bytes.length / length);
        for (int offset = 0; offset < bytes.length; offset += length) {
            records.add(Arrays.copyOfRange(bytes, offset, offset + length));
        }
        return records;
    }

    /** Writes a new file and fails if it already exists, so a run never overwrites an earlier generation. */
    static void writeNew(Path file, Collection<byte[]> records) {
        write(file, records, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE);
    }

    /** Replaces an existing dataset through a sibling temporary file and an atomic move. */
    static void replace(Path file, Collection<byte[]> records) {
        Path temp = file.resolveSibling(file.getFileName() + ".tmp");
        write(temp, records, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE);
        try {
            Files.move(temp, file, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE);
        } catch (IOException e) {
            throw new UncheckedIOException("cannot replace " + file, e);
        }
    }

    static void writeNewText(Path file, List<String> lines) {
        StringBuilder text = new StringBuilder();
        lines.forEach(line -> text.append(line).append('\n'));
        try {
            Files.write(file, text.toString().getBytes(StandardCharsets.ISO_8859_1), StandardOpenOption.CREATE_NEW);
        } catch (IOException e) {
            throw new UncheckedIOException("cannot write " + file, e);
        }
    }

    private static void write(Path file, Collection<byte[]> records, StandardOpenOption... options) {
        try (var out = Files.newOutputStream(file, options)) {
            for (byte[] record : records) {
                out.write(record);
            }
        } catch (IOException e) {
            throw new UncheckedIOException("cannot write " + file, e);
        }
    }

    private static byte[] readAll(Path file) {
        try {
            return Files.readAllBytes(file);
        } catch (IOException e) {
            throw new UncheckedIOException("cannot read " + file, e);
        }
    }
}
