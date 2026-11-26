package com.carddemo.programs.cbtrn02c.io;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
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
                if (l.length() > recordLength) {
                    throw new IOException("record longer than LRECL " + recordLength + " in " + path);
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
            Files.write(path, records, StandardCharsets.ISO_8859_1);
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
