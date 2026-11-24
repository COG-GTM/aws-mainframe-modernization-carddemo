package com.carddemo.migration;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/** Splits a sequential unload into fixed-length records. */
public final class RecordReader {

    private RecordReader() {
    }

    public static List<byte[]> read(InputStream in, int recordLength, SourceEncoding encoding) throws IOException {
        byte[] all = in.readAllBytes();
        return encoding == SourceEncoding.EBCDIC ? splitFixed(all, recordLength) : splitLines(all, recordLength);
    }

    static List<byte[]> splitFixed(byte[] all, int recordLength) {
        if (all.length % recordLength != 0) {
            throw new IllegalArgumentException("EBCDIC unload length " + all.length
                    + " is not a multiple of LRECL " + recordLength);
        }
        List<byte[]> records = new ArrayList<>(all.length / recordLength);
        for (int i = 0; i < all.length; i += recordLength) {
            records.add(Arrays.copyOfRange(all, i, i + recordLength));
        }
        return records;
    }

    static List<byte[]> splitLines(byte[] all, int recordLength) {
        String text = new String(all, StandardCharsets.ISO_8859_1);
        List<byte[]> records = new ArrayList<>();
        for (String line : text.split("\n")) {
            String record = line.endsWith("\r") ? line.substring(0, line.length() - 1) : line;
            if (record.isBlank()) {
                continue;
            }
            if (record.length() > recordLength) {
                if (!record.substring(recordLength).isBlank()) {
                    throw new IllegalArgumentException("Record longer than LRECL " + recordLength + ": " + record);
                }
                record = record.substring(0, recordLength);
            }
            records.add(String.format("%-" + recordLength + "s", record).getBytes(StandardCharsets.ISO_8859_1));
        }
        return records;
    }
}
