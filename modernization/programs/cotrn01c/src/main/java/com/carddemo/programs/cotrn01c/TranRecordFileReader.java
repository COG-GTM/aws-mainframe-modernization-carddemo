package com.carddemo.programs.cotrn01c;

import com.carddemo.programs.cotrn01c.copybook.TranRecord;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.List;

/** Reads fixed-width CVTRA05Y TRAN-RECORDs (one 350-byte record per line, ASCII display). */
public final class TranRecordFileReader {

    private TranRecordFileReader() {
    }

    public static List<TranRecord> readAll(Reader source) throws IOException {
        List<TranRecord> out = new ArrayList<>();
        try (BufferedReader r = new BufferedReader(source)) {
            String line;
            while ((line = r.readLine()) != null) {
                if (!line.isEmpty()) {
                    out.add(TranRecord.parse(line));
                }
            }
        }
        return out;
    }
}
