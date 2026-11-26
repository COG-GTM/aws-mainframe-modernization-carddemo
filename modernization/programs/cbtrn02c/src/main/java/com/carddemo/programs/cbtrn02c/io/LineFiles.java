package com.carddemo.programs.cbtrn02c.io;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.List;

/** Line-per-record text files (the app/data/ASCII layout); CR is stripped, short lines are space padded. */
public final class LineFiles {
    private LineFiles() {
    }

    public static List<String> readRecords(Path path, int recordLength) {
        try {
            List<String> out = new ArrayList<>();
            for (String line : Files.readAllLines(path, StandardCharsets.ISO_8859_1)) {
                String l = line.endsWith("\r") ? line.substring(0, line.length() - 1) : line;
                if (l.isEmpty()) {
                    continue;
                }
                out.add(fit(l, recordLength));
            }
            return out;
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    public static void writeRecords(Path path, List<String> records) {
        try {
            Path dir = path.toAbsolutePath().getParent();
            Path tmp = Files.createTempFile(dir, path.getFileName().toString(), ".tmp");
            try {
                Files.write(tmp, records, StandardCharsets.ISO_8859_1);
                Files.move(tmp, path, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE);
            } finally {
                Files.deleteIfExists(tmp);
            }
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    static String fit(String l, int recordLength) {
        if (l.length() >= recordLength) {
            return l.substring(0, recordLength);
        }
        return l + " ".repeat(recordLength - l.length());
    }
}
