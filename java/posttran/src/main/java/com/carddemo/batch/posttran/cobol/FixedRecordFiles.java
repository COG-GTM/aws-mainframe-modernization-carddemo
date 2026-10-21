package com.carddemo.batch.posttran.cobol;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/** Whole-file read/write of fixed-length record datasets (seed load and unload). */
public final class FixedRecordFiles {

    private FixedRecordFiles() {
    }

    public static List<String> read(Path path, RecordFormat format, int recordLength) {
        try {
            String content = new String(Files.readAllBytes(path), format.charset());
            List<String> records = new ArrayList<>();
            if (format.lineDelimited()) {
                for (String line : content.replace("\r", "").split("\n", -1)) {
                    records.add(line);
                }
                if (!records.isEmpty() && records.getLast().isEmpty()) {
                    records.removeLast();
                }
                return records.stream().map(r -> pad(r, recordLength, path)).toList();
            }
            if (content.length() % recordLength != 0) {
                throw new IllegalStateException(path + " length " + content.length()
                        + " is not a multiple of LRECL " + recordLength);
            }
            for (int i = 0; i < content.length(); i += recordLength) {
                records.add(content.substring(i, i + recordLength));
            }
            return records;
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    public static void write(Path path, RecordFormat format, int recordLength, List<String> records) {
        StringBuilder sb = new StringBuilder();
        for (String r : records) {
            if (r.length() != recordLength) {
                throw new IllegalArgumentException("Record length " + r.length() + " != " + recordLength);
            }
            sb.append(r);
            if (format.lineDelimited()) {
                sb.append('\n');
            }
        }
        try {
            if (path.getParent() != null) {
                Files.createDirectories(path.getParent());
            }
            Files.write(path, sb.toString().getBytes(format.charset()));
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    static String pad(String line, int recordLength, Object source) {
        if (line.length() > recordLength) {
            throw new IllegalStateException(source + ": record of " + line.length()
                    + " bytes exceeds LRECL " + recordLength);
        }
        return Pic.x(line, recordLength);
    }
}
