package com.carddemo.modernization.batch;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.PosixFileAttributeView;
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

    /**
     * Writes the new content of {@code file} to a new, uniquely named sibling temporary file with the dataset's
     * permissions and returns it. Only that file is removed if the write fails.
     */
    static Path stage(Path file, Collection<byte[]> records) {
        Path absolute = file.toAbsolutePath();
        Path temp;
        try {
            temp = Files.createTempFile(absolute.getParent(), absolute.getFileName() + ".", ".tmp");
        } catch (IOException e) {
            throw new UncheckedIOException("cannot stage " + file, e);
        }
        try {
            write(temp, records, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE);
            // After the write, so a read-only master can still be replaced (its directory decides that).
            if (Files.getFileStore(temp).supportsFileAttributeView(PosixFileAttributeView.class)
                    && Files.exists(absolute)) {
                Files.setPosixFilePermissions(temp, Files.getPosixFilePermissions(absolute));
            }
        } catch (IOException e) {
            UncheckedIOException failure = new UncheckedIOException("cannot stage " + file, e);
            deleteQuietly(temp, failure);
            throw failure;
        } catch (RuntimeException e) {
            deleteQuietly(temp, e);
            throw e;
        }
        return temp;
    }

    /** Removes a staged file that will not be committed, recording a failure to do so on {@code cause}. */
    static void deleteQuietly(Path staged, Exception cause) {
        try {
            Files.deleteIfExists(staged);
        } catch (IOException e) {
            cause.addSuppressed(e);
        }
    }

    /** Moves a staged file over the dataset it replaces. */
    static void commit(Path staged, Path file) {
        try {
            Files.move(staged, file, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE);
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
