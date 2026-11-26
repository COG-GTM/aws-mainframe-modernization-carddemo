package com.carddemo.programs.cotrn00c;

import com.carddemo.programs.cotrn00c.copybook.TranRecord;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.List;

/** Reads fixed-width 350-byte TRAN-RECORD lines (one record per line, ASCII). */
public final class TranRecordReader {

    private TranRecordReader() {
    }

    public static List<TranRecord> readAll(Reader in) throws IOException {
        List<TranRecord> out = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(in)) {
            String line;
            while ((line = br.readLine()) != null) {
                if (!line.isBlank()) {
                    out.add(TranRecord.parse(line));
                }
            }
        }
        return out;
    }
}
