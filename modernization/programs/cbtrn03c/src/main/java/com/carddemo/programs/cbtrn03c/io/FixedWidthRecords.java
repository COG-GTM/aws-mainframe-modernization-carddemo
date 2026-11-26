package com.carddemo.programs.cbtrn03c.io;

import com.carddemo.programs.cbtrn03c.support.Fixed;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/** Loads fixed-width records (one byte per character, ISO-8859-1) from a data set. */
public final class FixedWidthRecords {
    public static final Charset CHARSET = StandardCharsets.ISO_8859_1;

    private FixedWidthRecords() {
    }

    public static List<String> load(Path path, int lrecl, RecordFormat format) throws IOException {
        String content = Files.readString(path, CHARSET);
        List<String> records = new ArrayList<>();
        if (format == RecordFormat.FIXED) {
            for (int i = 0; i < content.length(); i += lrecl) {
                records.add(Fixed.pic(content.substring(i, Math.min(content.length(), i + lrecl)), lrecl));
            }
            return records;
        }
        for (String line : content.split("\n", -1)) {
            if (line.endsWith("\r")) {
                line = line.substring(0, line.length() - 1);
            }
            records.add(Fixed.pic(line, lrecl));
        }
        if (content.isEmpty() || content.endsWith("\n")) {
            records.remove(records.size() - 1);
        }
        return records;
    }
}
