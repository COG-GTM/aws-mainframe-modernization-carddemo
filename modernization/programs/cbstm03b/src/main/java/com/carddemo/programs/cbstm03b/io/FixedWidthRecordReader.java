package com.carddemo.programs.cbstm03b.io;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Reads a RECFM=FB dataset exported as line-delimited text (app/data/ASCII). Trailing blanks
 * stripped by the export are restored by space-padding to LRECL; longer lines are rejected.
 */
public final class FixedWidthRecordReader {

    private final int lrecl;

    public FixedWidthRecordReader(int lrecl) {
        this.lrecl = lrecl;
    }

    public List<String> readAll(Path path) throws IOException {
        try (Reader r = Files.newBufferedReader(path, StandardCharsets.US_ASCII)) {
            return readAll(r);
        }
    }

    public List<String> readAll(Reader reader) throws IOException {
        BufferedReader br = reader instanceof BufferedReader b ? b : new BufferedReader(reader);
        List<String> out = new ArrayList<>();
        String line;
        int lineNo = 0;
        while ((line = br.readLine()) != null) {
            lineNo++;
            if (line.isEmpty()) {
                continue;
            }
            if (line.length() > lrecl) {
                throw new RecordLengthException("line " + lineNo + " is " + line.length() + " bytes, LRECL=" + lrecl);
            }
            out.add(line + " ".repeat(lrecl - line.length()));
        }
        return out;
    }
}
