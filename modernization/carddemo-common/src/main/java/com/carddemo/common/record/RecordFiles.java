package com.carddemo.common.record;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/** Whole-file record I/O for the two physical formats of the stream's data. */
public final class RecordFiles {

    private RecordFiles() {
    }

    /**
     * Line-sequential text ({@code app/data/ASCII}, GnuCOBOL unloads): CR stripped, each line right-padded with spaces to
     * {@code length} (GnuCOBOL drops trailing spaces on write). A line longer than {@code length} is an error.
     */
    public static List<String> readLines(Path file, int length, Charset charset) {
        try {
            List<String> records = new ArrayList<>();
            int lineNo = 0;
            for (String line : Files.readAllLines(file, charset)) {
                lineNo++;
                String l = line.replace("\r", "");
                if (l.length() > length) {
                    throw new IllegalArgumentException(file + " line " + lineNo + " is " + l.length() + " characters (> " + length + ")");
                }
                records.add(l + " ".repeat(length - l.length()));
            }
            return records;
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    /** Line-sequential output the way GnuCOBOL writes it: trailing spaces removed, LF after each record. */
    public static void writeLines(Path file, List<String> records, Charset charset) {
        StringBuilder sb = new StringBuilder();
        for (String r : records) {
            sb.append(r.stripTrailing()).append('\n');
        }
        try {
            Files.writeString(file, sb, charset);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    /** RECFM=F/FB: consecutive {@code length}-byte records, no separators; a short last record is an error. */
    public static List<String> readFixed(Path file, int length, Charset charset) {
        try {
            byte[] bytes = Files.readAllBytes(file);
            if (bytes.length % length != 0) {
                throw new IllegalArgumentException(file + " is " + bytes.length + " bytes, not a multiple of " + length);
            }
            List<String> records = new ArrayList<>(bytes.length / length);
            for (int i = 0; i < bytes.length; i += length) {
                records.add(new String(bytes, i, length, charset));
            }
            return records;
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}
